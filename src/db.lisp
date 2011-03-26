(cl:defpackage #:sykosomatic.db
  (:use :cl :alexandria :chillax.core :chillax.jsown)
  (:export :init-db :mkdoc :doc-val :ensure-doc :get-uuid :couchfun :mapfun :view-query-value
           :assert-validation :with-validation :assert-required :*db*))
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

(defmacro mapfun (doc-name target-types &body body)
  (let ((type-var (gentemp "TYPE"))
        (target-types (ensure-list target-types)))
    `(couchfun (,doc-name &aux (,type-var (,(intern "HASHGET") ,doc-name "type")))
       (,(intern "WHEN") (,(intern "OR")
                           ,@(mapcar (lambda (type) `(equal ,type-var ,type))
                                     target-types))
         ,@body))))

(defmacro couchfun (lambda-list &body body)
  `(prin1-to-string '(lambda ,lambda-list ,@body)))

(defun view-query-value (design-doc-name view-name key &optional (singlep t))
  (when-let ((results (doc-val (query-view *db* design-doc-name view-name
                                           :key key)
                               "rows")))
    (if singlep
        (doc-val (car results) "value")
        (mapcar (rcurry #'doc-val "value") results))))

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
