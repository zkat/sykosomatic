(util:def-file-package #:sykosomatic.account
  (:use :cl-ppcre
        :sykosomatic.db
        :sykosomatic.util.form)
  (:export :create-account :find-account :find-account-by-email :validate-account
           :add-body :remove-body :deactivate-body :activate-body :account-bodies
           :account-email :account-display-name :signup))

;;;
;;; Utils
;;;

;;; The password confirmation scheme uses PBKDF2.  The two parameters should be adjusted according
;;; to your server's capabilities. Do some profiling and make these numbers as high as you can
;;; without lagging under heavy load.

(defparameter *key-derivation-iterations* 720)
(defparameter *key-length* 32)
(defun hash-password (password salt
                      &key
                        (key-iterations *key-derivation-iterations*)
                        (key-length *key-length*))
  "Password hashing function."
  (ironclad:derive-key
   (make-instance 'ironclad:pbkdf2 :digest :sha256)
   (ironclad:ascii-string-to-byte-array password)
   salt
   key-iterations
   key-length))

(defun gensalt () (random-byte-array *key-length*))

;;;
;;; DB
;;;
(defun account-email (acc-id)
  (db-query (:select 'email :from 'account :where (:= 'id acc-id))
            :single))

(defdao account ()
  ((display-name text)
   (email text)
   (password bytea)
   (salt bytea)
   (key-iterations integer)
   (key-length integer)
   (created-at timestamp :col-default (:now)))
  (:keys id)
  (:unique-index id)
  (:unique-index email)
  (:unique email)
  (:unique display-name))

(defun account-exists-p (email)
  (values
   (db-query (:select t :from 'account :where (:= 'email (string-downcase email)))
             :single)))

(defun validate-account (email password)
  (cmatch (db-query (:select 'id 'password 'salt 'key-iterations 'key-length
                             :from 'account
                             :where (:= 'email email))
                    :row)
    ((list id account-pass salt *key-derivation-iterations* *key-length*)
     (when (equalp account-pass (hash-password password salt))
       id))
    (_
     nil)))

(defun display-name-exists-p (display-name)
  (values
   (db-query (:select t :from 'account :where (:= 'display-name display-name))
             :single)))

(defun change-password (account-id new-password)
  (let ((salt (gensalt)))
    (values
     (db-query (:update 'account
                        :set
                        'salt salt
                        'password (hash-password new-password salt)
                        'key-iterations *key-derivation-iterations*
                        'key-length *key-length*
                        :where (:= 'id account-id))))))

;;;
;;; Bodies
;;;
(defdao account-body ()
  ((entity-id bigint)
   (account-id bigint)
   (activep boolean :col-default t))
  (:keys entity-id account-id)
  (:unique (entity-id account-id))
  (:unique-index entity-id account-id))

(defun add-body (entity-id account-id)
  (insert-row 'account-body
              :entity-id entity-id
              :account-id account-id))

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

(defun field-required (validator)
  (lambda (&rest args)
    (check-field (not (emptyp (car args))) "Field is required.")
    (apply validator args)))

(defun valid-email (email)
  (check-field (scan *email-regex* email) "Invalid email.")
  (check-field (not (account-exists-p email)) "Account already exists.")
  email)

(defun valid-password (password)
  (check-field (>= (length password) 6) "Must be at least 6 characters long.")
  password)
(defun valid-confirmation (confirm)
  (check-field (string= confirm (field-raw-value *form* :password))
               "Confirmation and password must match.")
  confirm)

(defun valid-display-name (display-name)
  (check-field (and (>= (length display-name) 4)
                    (<= (length display-name) 32))
               "Must be between 4 and 32 characters long.")
  (check-field (scan *display-name-regex* display-name)
               "Only alphabetic characters, digits, and the characters '.', '_', and '-' are allowed.")
  (check-field (not (display-name-exists-p display-name)) "Display name already exists.")
  display-name)

(deform signup ()
  ((:email (field-required 'valid-email))
   (:display-name (field-required 'valid-display-name))
   (:password (field-required 'valid-password))
   (:confirmation (field-required (compose 'valid-confirmation 'valid-password)))))

(defun create-account (form)
  (assert (form-valid-p form) () "Invalid form passed to CREATE-ACCOUNT.")
  (let ((salt (gensalt)))
    (insert-row 'account
                :email (field-value form :email)
                :display-name (field-value form :display-name)
                :password (hash-password (field-value form :password) salt)
                :salt salt
                :key-iterations *key-derivation-iterations*
                :key-length *key-length*)))
