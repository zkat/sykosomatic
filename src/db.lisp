(cl:defpackage #:sykosomatic.db
  (:use :cl :alexandria :chillax.core :chillax.jsown)
  (:export :init-db :mkdoc :doc-val :ensure-doc :get-uuid :couchfun :view-query-value))
(cl:in-package #:sykosomatic.db)

(defparameter *server* (make-instance 'jsown-server))
(defvar *db* nil)

(defun init-db ()
  (setf *db* (ensure-db *server* "sykosomatic")))

(defun mkdoc (&rest keys-and-values)
  (cons :obj (plist-alist keys-and-values)))

(defun doc-val (document key)
  (jsown:val document key))
(defun (setf doc-val) (new-value document key)
  (setf (jsown:val document key) new-value)
  new-value)

(defun ensure-doc (id document)
  (handler-case
      (put-document *db* id document)
    (document-conflict ()
      (ensure-doc id (progn
                       (setf (doc-val document "_rev")
                             (get-document-revision *db* id))
                       document)))))

(defun get-uuid ()
  (car (doc-val (get-uuids *server* :number 1) "uuids")))

(defmacro couchfun (lambda-list &body body)
  (prin1-to-string `(lambda ,lambda-list ,@body)))

(defun view-query-value (design-doc-name view-name key)
  (when-let ((results (doc-val (query-view *db* design-doc-name view-name
                                           :key key)
                               "rows")))
    (doc-val (car results) "value")))
