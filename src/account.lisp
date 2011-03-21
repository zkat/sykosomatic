(cl:defpackage #:sykosomatic.account
  (:use :cl :alexandria :cl-ppcre :sykosomatic.db)
  (:export :ensure-account-design-doc :create-account :find-account :validate-credentials
           :account-name
           :account-display-name))
(cl:in-package #:sykosomatic.account)

(declaim (optimize debug))

;;;
;;; Utils
;;;
(defun hash-password (password)
  "Password hashing function."
  ;; TODO - maybe a salt?
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array
     password))))

;;;
;;; Design and querying
;;;
(defun ensure-account-design-doc ()
  (ensure-doc "_design/account"
              (mkdoc "language" "common-lisp"
                     "views" (mkdoc "by_account_name"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (string-downcase (hashget doc "account_name"))
                                                     doc))))
                                    "by_account_name_password"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (list (string-downcase (hashget doc "account_name"))
                                                           (hashget doc "password"))
                                                     doc))))
                                    "by_display_name"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (string-downcase (hashget doc "display_name"))
                                                     doc))))))))

(defun account-view-value (view-name key)
  (view-query-value "account" view-name key))

(defun find-account-by-display-name (display-name)
  (account-view-value "by_display_name" display-name))

(defun find-account (account-name)
  (account-view-value "by_account_name" (string-downcase account-name)))

(defun validate-credentials (account-name password &aux (hashed-pass (hash-password password)))
  (account-view-value "by_account_name_password" (list (string-downcase account-name) hashed-pass)))

(defun account-name (account)
  (doc-val account "account_name"))
(defun account-display-name (account)
  (doc-val account "display_name"))

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
       (<= (length password) 32)
       (not (find-if-not #'standard-char-p password))))

(defun valid-display-name-p (display-name)
  (when (and (>= (length display-name) 4)
             (<= (length display-name) 32)
             (scan *display-name-regex* display-name))
    t))

(defun validate-new-account (account-name display-name password confirmation)
  (with-validation
    (assert-required "Account name" account-name)
    (assert-required "Display Name" display-name)
    (assert-required "Password" password)
    (assert-required "Confirmation" confirmation)
    (assert-validation (valid-email-p account-name) "Invalid email.")
    (assert-validation (not (find-account account-name)) "Account already exists.")
    (assert-validation (valid-password-p password) "Invalid password.")
    (assert-validation (valid-display-name-p display-name) "Invalid display name.")
    (assert-validation (not (find-account-by-display-name display-name)) "Display name already in use.")
    (assert-validation (string= password confirmation) "Password confirmation does not match.")))

(defun create-account (account-name display-name password confirmation
                       &aux (hashed-pass (hash-password password)))
  (multiple-value-bind (validp errors)
      (validate-new-account account-name display-name password confirmation)
    (if validp
        (ensure-doc (get-uuid)
                    (mkdoc "type" "account"
                           "account_name" account-name
                           "display_name" display-name
                           "password" hashed-pass
                           "created_at" (get-universal-time)))
        (values nil errors))))
