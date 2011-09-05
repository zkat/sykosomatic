(cl:defpackage #:sykosomatic.account
  (:use :cl :alexandria :cl-ppcre :sykosomatic.db :sykosomatic.utils :postmodern)
  (:export :create-account :find-account :find-account-by-email :validate-account
           :account-email :account-display-name))
(cl:in-package #:sykosomatic.account)

(declaim (optimize debug))

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
  (ironclad:byte-array-to-hex-string
   (ironclad:derive-key
    (make-instance 'ironclad:pbkdf2 :digest :sha256)
    (ironclad:ascii-string-to-byte-array password)
    (ironclad:ascii-string-to-byte-array salt)
    *key-derivation-iterations*
    *key-length*)))

;;;
;;; DB
;;;
(defgeneric account-email (account))
(defgeneric account-display-name (account))
(defgeneric account-password (account))
(defgeneric account-password-salt (account))

(defmethod account-email ((acc-id integer))
  (query (:select 'email :from 'account :where (:= 'id acc-id))
         :single))
(defmethod account-display-name ((acc-id integer))
  (query (:select 'display-name :from 'account :where (:= 'id acc-id))
         :single))

(defdao account ()
  ((id :col-type serial :reader id)
   (display-name :col-type text :initarg :display-name :reader account-display-name)
   (email :col-type text :initarg :email :reader account-email)
   (password :col-type text :initarg :password :reader account-password)
   (salt :col-type text :initarg :salt :reader account-password-salt)
   (created-at :col-type timestamp :col-default (:now)))
  (:keys id)
  (:unique-index 'id)
  (:unique-index 'email)
  (:unique 'email)
  (:unique 'display-name))

(defun find-account (id)
  (with-db ()
    (get-dao 'account id)))

(defun find-account-by-email (email)
  (with-db ()
    (car (select-dao 'account (:= 'email (string-downcase email))))))

(defun validate-account (email password)
  (with-db ()
    (with-transaction ()
      (when-let (account (find-account email))
        (let ((hashed-pass (hash-password password (account-password-salt account))))
          (when (string= hashed-pass (account-password account))
            account))))))

(defun display-name-exists-p (display-name)
  (with-db ()
    (query (:select t :from 'account :where (:= 'display-name display-name))
           :single)))

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
       (not (find-if-not #'standard-char-p password))))

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
    (assert-validation (not (find-account email)) "Account already exists.")
    (assert-validation (valid-password-p password) "Password must be at least 6 characters long and can't contain funky characters.")
    (assert-validation (valid-display-name-p display-name) "Invalid display name. Display name must be between 4 and 32 alphanumeric characters.")
    (assert-validation (not (display-name-exists-p display-name)) "Display name already in use.")
    (assert-validation (string= password confirmation) "Password confirmation does not match.")))

(defun create-account (email display-name password confirmation)
  (with-db ()
    (with-transaction ()
      (multiple-value-bind (validp errors)
          (validate-new-account email display-name password confirmation)
        (if validp
            (let ((salt (random-string 32)))
              (make-dao 'account
                        :email email
                        :display-name display-name
                        :password (hash-password password salt)
                        :salt salt))
            (values nil errors))))))
