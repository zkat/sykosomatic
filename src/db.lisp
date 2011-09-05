(cl:defpackage #:sykosomatic.db
  (:use :cl :alexandria :postmodern)
  (:export :id :defdao :init-db :with-db :dblog :rebuild :drop-table :drop-all-tables
           :assert-validation :with-validation :assert-required))
(cl:in-package #:sykosomatic.db)

(defparameter *db-name* "sykosomatic")
(defparameter *db-user* "postgres")
(defparameter *db-password* "")
(defparameter *db-host* "localhost")

;;; SQL utils
(defun pomo::\!unique (&rest target-fields &aux (target-fields (mapcar #'pomo::to-sql-name target-fields)))
  (format nil "ALTER TABLE ~A ADD CONSTRAINT ~A UNIQUE (~{~A~^, ~})"
          (pomo::to-sql-name *table-name*)
          (pomo::to-sql-name (format nil "~A_~{~A~^_~}_unique" *table-name* target-fields))
          target-fields))
(export 'pomo::\!unique (find-package :postmodern))

(defmacro defdao (name superclasses slots &body dao-options)
  (flet ((parsed-opts (keyword deftable-func-name)
           (mapcar (lambda (statement)
                     `(,deftable-func-name ,@(cdr statement)))
                   (remove-if-not (lambda (opt) (eq keyword (car opt)))
                                  dao-options))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         (:metaclass dao-class)
         ,@(when-let (keys (assoc :keys dao-options)) `(,keys)))
       (deftable ,name
         (!dao-def)
         ,@(parsed-opts :index '!index)
         ,@(parsed-opts :unique-index '!unique-index)
         ,@(parsed-opts :foreign-key '!foreign)
         ,@(parsed-opts :unique '!unique)))))

(defun drop-table (symbol)
  (query (format nil "drop table if exists ~A" (sql-compile symbol))))
(defun drop-all-tables ()
  (map nil (compose #'drop-table #'car) pomo::*tables*))

(defun rebuild ()
  (drop-all-tables)
  (create-all-tables))

(defun dblog (format-string &rest format-args)
  (format t "~&LOG - ~A~%" (apply #'format nil format-string format-args)))

(defun init-db ()
  (rebuild))

(defmacro with-db (() &body body)
  `(with-connection (*db-name* *db-user* *db-password* *db-host*)
     ,@body))

(defgeneric id (dao))

;; Forms
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
