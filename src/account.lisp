(cl:defpackage #:belletrist.account
  (:use :cl :alexandria :chillax.core :chillax.jsown)
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
                     "views" (mkdoc "by_username"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (string-downcase (hashget doc "username"))
                                                     doc))))
                                    "password_validation"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (list (string-downcase (hashget doc "username"))
                                                           (hashget doc "password"))
                                                     doc))))))
              (mkdoc "language" "javascript"
                     "views" (mkdoc "by_username"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit(doc.username.toLowerCase(),doc);
                                       }
                                     }")
                                    "by_username_password"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit([doc.username.toLowerCase(),doc.password],doc);
                                       }
                                     }")))))

(defun create-account (username password &key errorp &aux (hashed-pass (hash-password password)))
  (if-let ((existing-account (find-account username)))
    (when errorp
      (cerror "Change password instead." "Account already exists.")
      (setf (doc-val existing-account "password") hashed-pass)
      (put-document *db* (doc-val existing-account "_id") existing-account))
    (ensure-doc (get-uuid)
                (mkdoc "type" "account"
                       "username" username
                       "password" hashed-pass))))

(defun find-account (username)
  (when-let ((results (doc-val (query-view *db* "account" "by_username"
                                           :key (string-downcase username))
                               "rows")))
    (doc-val (car results)
             "value")))

(defun validate-credentials (username password &aux (hashed-pass (hash-password password)))
  (when-let ((results (doc-val (query-view *db* "account" "by_username_password"
                                           :key (list (string-downcase username) hashed-pass))
                               "rows")))
    (doc-val (car results)
             "value")))
