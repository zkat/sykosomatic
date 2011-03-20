(cl:defpackage #:belletrist.account
  (:use :cl :alexandria :chillax.core :chillax.jsown)
  (:export :boot-db))
(cl:in-package #:belletrist.account)

(declaim (optimize debug))
;; TODO
;; * Create an account object based on stored version.
;; * Encrypt user passwords.
;; * Fix the goddamn view server.
;; * Build a view server that uses jsown.
;; * Convert view code to use jsown, too.

(defparameter *server* (make-instance 'jsown-server))
(defparameter *db* (ensure-db *server* "belletrist"))

(defun boot-db ()
  ;; First-time boot stuff goes here.
  (ensure-account-design-doc))

(defun mkdoc (&rest keys-and-values)
  (cons :obj (plist-alist keys-and-values)))

(defun ensure-doc (id document)
  (handler-case
      (put-document *db* id document)
    (document-conflict ()
      (ensure-design-doc id (let ((new-rev (get-document-revision *db* id))
                                  (old-rev-cons (assoc "_rev" (cdr document) :test #'equal)))
                              (if old-rev-cons
                                  (setf (cdr old-rev-cons) new-rev)
                                  (push (cons "_rev" new-rev)
                                        (cdr document)))
                              document)))))

(defun doc-val (document key)
  (jsown:val document key))

(defun get-uuid ()
  (cadr (assoc "uuids" (cdr (get-uuids *server* :number 1)) :test #'string=)))

(defun create-account (username password)
  (ensure-doc (get-uuid)
              (mkdoc "type" "account"
                     "username" username
                     "password" password)))

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
                                               (emit (hashget doc "username")
                                                     doc))))
                                    "password_validation"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (when (equal type "account")
                                               (emit (list (hashget doc "username")
                                                           (hashget doc "password"))
                                                     doc))))))
              (mkdoc "language" "javascript"
                     "views" (mkdoc "by_username"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit(doc.username,doc);
                                       }
                                     }")
                                    "password_validation"
                                    (mkdoc "map"
                                     "function (doc) {
                                       if (doc.type == 'account') {
                                         emit([doc.username,doc.password],true);
                                       }
                                     }")))))

(defun find-account (username)
  (let ((results (doc-val (query-view *db* "account" "by_username" :key username) "rows")))
    (when results
      (doc-val (car results)
               "value"))))

(defun validate-account (username password)
  (let ((results (doc-val (query-view *db* "account" "password_validation" :key (list username password)) "rows")))
    (when results
      (doc-val (car results)
               "value"))))
