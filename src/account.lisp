(cl:defpackage #:sykosomatic.account
  (:use :cl :alexandria :cl-ppcre :sykosomatic.db :sykosomatic.util)
  (:export :create-account :find-account :find-account-by-email :validate-account
           :add-body :remove-body :deactivate-body :activate-body :account-bodies
           :account-email :account-display-name))
(cl:in-package #:sykosomatic.account)

;;;
;;; Utils
;;;

;;; The password confirmation scheme uses PBKDF2.  The two parameters should be adjusted according
;;; to your server's capabilities. Do some profiling and make these numbers as high as you can
;;; without lagging under heavy load.

(defparameter *key-derivation-iterations* 1056)
(defparameter *key-length* 32)
(defun hash-password (password salt)
  "Password hashing function."
  (ironclad:derive-key
   (make-instance 'ironclad:pbkdf2 :digest :sha256)
   (ironclad:ascii-string-to-byte-array password)
   salt
   *key-derivation-iterations*
   *key-length*))

(defun gensalt (&key (length 32) (size 256))
  (map-into (make-array length :element-type '(unsigned-byte 8))
            (curry #'random size)))

;;;
;;; DB
;;;
(defgeneric account-email (account))
(defgeneric account-display-name (account))
(defgeneric account-password (account))
(defgeneric account-password-salt (account))

(defmethod account-email ((acc-id integer))
  (db-query (:select 'email :from 'account :where (:= 'id acc-id))
            :single))
(defmethod account-display-name ((acc-id integer))
  (db-query (:select 'display-name :from 'account :where (:= 'id acc-id))
            :single))

(defdao account ()
  ((display-name text :reader account-display-name)
   (email text :reader account-email)
   (password bytea :reader account-password)
   (salt bytea :reader account-password-salt)
   (created-at timestamp :col-default (:now)))
  (:keys id)
  (:unique-index id)
  (:unique-index email)
  (:unique email)
  (:unique display-name))

(defun find-account (id)
  (with-db ()
    (get-dao 'account id)))

(defun find-account-by-email (email)
  (with-db ()
    (car (select-dao 'account (:= 'email (string-downcase email))))))

(defun validate-account (email password)
  (when-let (account (find-account-by-email email))
    (let ((hashed-pass (hash-password password (account-password-salt account))))
      (when (equalp hashed-pass (account-password account))
        account))))

(defun display-name-exists-p (display-name)
  (db-query (:select t :from 'account :where (:= 'display-name display-name))
            :single))

;;;
;;; Bodies
;;;
(defdao account-body ()
  ((entity-id bigint)
   (account-id bigint)
   (activep boolean :col-default t))
  (:keys entity-id account-id)
  (:unique entity-id account-id)
  (:unique-index entity-id account-id))

(defun add-body (entity-id account-id)
  (with-db ()
    (id (make-dao 'account-body
                  :entity-id entity-id
                  :account-id account-id))))

(defun remove-body (entity-id account-id)
  (db-query (:delete-from 'account-body
                          :where (:and (:= 'entity-id entity-id)
                                       (:= 'account-id account-id)))))

(defun deactivate-body (entity-id account-id)
  (db-query (:update 'account-body
                     :set 'activep nil
                     :where (:and (:= 'entity-id entity-id)
                                  (:= 'account-id account-id)))))

(defun activate-body (entity-id account-id)
  (db-query (:update 'account-body
                     :set 'activep t
                     :where (:and (:= 'entity-id entity-id)
                                  (:= 'account-id account-id)))))

(defun account-bodies (account-id)
  (db-query (:select 'entity-id :from 'account-body
                     :where (:and 'activep
                                  (:= 'account-id account-id)))
            :column))

;;;
;;; Creation and validation
;;;
(defparameter *email-regex* (create-scanner "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$"
                                            :case-insensitive-mode t))

(defparameter *display-name-regex* (create-scanner "^[A-Z0-9._.-]+$"
                                                   :case-insensitive-mode t))

(defun valid-email-p (email)
  (when (scan *email-regex* email)
    t))

(defun valid-password-p (password)
  (and (>= (length password) 6)
       (every #'standard-char-p password)))

(defun valid-display-name-p (display-name)
  (when (and (>= (length display-name) 4)
             (<= (length display-name) 32)
             (scan *display-name-regex* display-name))
    t))

(defun validate-new-account (email display-name password confirmation)
  (with-validation
    (assert-required "Email" email)
    (assert-required "Display Name" display-name)
    (assert-required "Password" password)
    (assert-required "Confirmation" confirmation)
    (assert-validation (valid-email-p email) "Invalid email.")
    (assert-validation (not (find-account-by-email email)) "Account already exists.")
    (assert-validation (valid-password-p password) "Password must be at least 6 characters long and can't contain funky characters.")
    (assert-validation (valid-display-name-p display-name) "Invalid display name. Display name must be between 4 and 32 alphanumeric characters.")
    (assert-validation (not (display-name-exists-p display-name)) "Display name already in use.")
    (assert-validation (string= password confirmation) "Password confirmation does not match.")))

(defun create-account (email display-name password confirmation)
  (with-transaction ()
    (multiple-value-bind (validp errors)
        (validate-new-account email display-name password confirmation)
      (if validp
          (let ((salt (gensalt)))
            (make-dao 'account
                      :email email
                      :display-name display-name
                      :password (hash-password password salt)
                      :salt salt))
          (values nil errors)))))
