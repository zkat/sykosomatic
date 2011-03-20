(cl:defpackage #:belletrist.account
  (:use :cl :alexandria :chillax.core :chillax.jsown :cl-ppcre)
  (:export :boot-db :create-account :find-account :validate-credentials))
(cl:in-package #:belletrist.account)

(declaim (optimize debug))

(defparameter *server* (make-instance 'jsown-server))
(defparameter *db* (ensure-db *server* "belletrist"))

(defun hash-password (password)
  "Password hashing function."
  ;; TODO - maybe a salt?
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array
     password))))

(defun boot-db ()
  ;; First-time boot stuff goes here.
  (ensure-account-design-doc))

(defun mkdoc (&rest keys-and-values)
  (cons :obj (plist-alist keys-and-values)))

(defun ensure-doc (id document)
  (handler-case
      (put-document *db* id document)
    (document-conflict ()
      (ensure-doc id (let ((new-rev (get-document-revision *db* id))
                           (old-rev-cons (assoc "_rev" (cdr document) :test #'equal)))
                       (if old-rev-cons
                           (setf (cdr old-rev-cons) new-rev)
                           (push (cons "_rev" new-rev)
                                 (cdr document)))
                       document)))))

(defun doc-val (document key)
  (jsown:val document key))
(defun (setf doc-val) (new-value document key)
  (setf (jsown:val document key) new-value)
  new-value)

(defun get-uuid ()
  (car (doc-val (get-uuids *server* :number 1) "uuids")))

(defmacro couchfun (lambda-list &body body)
  (prin1-to-string `(lambda ,lambda-list ,@body)))

(defun ensure-account-design-doc ()
  (ensure-doc "_design/account"
              ;; View server is fucked? :(
              #+nil(mkdoc "language" "common-lisp"
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
                                                     doc))))))
              (mkdoc "language" "javascript"
                     "views" (mkdoc "by_account_name"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit(doc.account_name.toLowerCase(),doc);
                                       }
                                     }")
                                    "by_account_name_password"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit([doc.account_name.toLowerCase(),doc.password],doc);
                                       }
                                     }")))))

(defparameter *email-regex* (create-scanner "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$"
                                            :case-insensitive-mode t))

(defparameter *display-name-regex* (create-scanner "^[A-Z_\\-.']$"
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

(defvar *validation-errors*)

(defmacro assert-validation (test failure-message)
  `(%assert-validation (lambda () ,test) ,failure-message))

(defmacro with-validation (&body body)
  `(let (*validation-errors*)
     ,@body
     (if *validation-errors*
         (values nil (nreverse *validation-errors*))
         t)))

(defun %assert-validation (test failure-message)
  (unless (funcall test)
    (push failure-message *validation-errors*)))

(defun assert-required (fieldname  x)
  (assert-validation (not (emptyp x)) (format nil "~A is required." fieldname)))

(defun validate-new-account (account-name password confirmation)
  (with-validation
    (assert-required "Account name" account-name)
    (assert-required "Password" password)
    (assert-required "Confirmation" confirmation)
    (assert-validation (valid-email-p account-name) "Invalid email.")
    (assert-validation (not (find-account account-name)) "Account already exists.")
    (assert-validation (valid-password-p password) "Invalid password.")
    (assert-validation (string= password confirmation) "Password confirmation does not match.")))

(defun create-account (account-name password confirmation
                       &aux (hashed-pass (hash-password password)))
  (multiple-value-bind (validp errors)
      (validate-new-account account-name password confirmation)
    (if validp
        (ensure-doc (get-uuid)
                    (mkdoc "type" "account"
                           "account_name" account-name
                           "password" hashed-pass))
        (values nil errors))))

(defun find-account (account-name)
  (when-let ((results (doc-val (query-view *db* "account" "by_account_name"
                                           :key (string-downcase account-name))
                               "rows")))
    (doc-val (car results)
             "value")))

(defun validate-credentials (account-name password &aux (hashed-pass (hash-password password)))
  (when-let ((results (doc-val (query-view *db* "account" "by_account_name_password"
                                           :key (list (string-downcase account-name) hashed-pass))
                               "rows")))
    (doc-val (car results)
             "value")))
