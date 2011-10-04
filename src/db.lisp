(util:def-file-package #:sykosomatic.db
  (:use :postmodern)
  (:shadow :with-transaction)
  (:export
   ;; DAO
   :id :defdao :get-dao :select-dao :make-dao
   ;; Misc sql
   :defprepared
   ;; Querying
   :query :db-query :doquery :insert-row
   ;; Connections
   :with-db :with-transaction
   :get-connection
   :done-with-connection
   ;; Development utilities
   :dblog :init-db :rebuild-db :rebuild-table
   :drop-table :drop-all-tables
   ;; Init hooks
   :register-db-init-hook :remove-db-init-hook :run-db-init-hook
   ;; Validation
   :assert-validation :with-validation :assert-required
   ;; misc stuff from s-sql
   :smallint :bigint :numeric :real :double-precision :bytea
   :text :varchar :db-null :sql-type-name :*standard-sql-strings*
   :sql-escape-string :sql-escape :from-sql-name :to-sql-name
   :*escape-sql-names-p* :sql :sql-compile :sql-template :$$
   :register-sql-operators :enable-s-sql-syntax :sql-error))

(defparameter *db-name* "sykosomatic")
(defparameter *db-user* "postgres")
(defparameter *db-password* "")
(defparameter *db-host* "localhost")

;;;
;;; Connection pooling
;;;
(defparameter *connection-pool-lock* (bt:make-lock))
(defparameter *max-pooled-connections* 50)
(defvar *connection-pool* (make-queue *max-pooled-connections*))

(defun get-connection ()
  (or (bt:with-lock-held (*connection-pool-lock*)
        (unless (queue-empty-p *connection-pool*)
          (dequeue *connection-pool*)))
      (apply #'connect (list *db-name* *db-user* *db-password* *db-host*))))

(defun done-with-connection (connection)
  (bt:with-lock-held (*connection-pool-lock*)
    (if (queue-full-p *connection-pool*)
        (disconnect connection)
        (enqueue connection *connection-pool*))))

(defun clear-pooled-connections ()
  (bt:with-lock-held (*connection-pool-lock*)
    (loop until (queue-empty-p *connection-pool*)
       for connection = (dequeue *connection-pool*)
       when (connected-p connection)
       do (disconnect connection))))

(defmacro with-db ((&key (reusep t)) &body body)
  `(let* ((reusing-connection-p (and *database* ,reusep))
          (*database* (or (when reusing-connection-p *database*)
                          (get-connection))))
     (unwind-protect (progn ,@body)
       (unless reusing-connection-p
         (done-with-connection *database*)))))

(defmacro db-query (query &rest args/format)
  `(with-db () (query ,query ,@args/format)))

;;;
;;; SQL utils
;;;
(defclass base-table ()
  ((id :reader id :col-type serial))
  (:metaclass dao-class))

(defmacro defdao (name superclasses slots &body dao-options)
  (flet ((parsed-opts (keyword deftable-func-name)
           (mapcar (lambda (statement)
                     `(,deftable-func-name ,@(cdr statement)))
                   (remove-if-not (lambda (opt) (eq keyword (car opt)))
                                  dao-options))))
    `(progn
       (defclass ,name ,(append '(base-table) superclasses)
         ,(loop for (slotname col-type . other-args) in slots
             collect `(,slotname :initarg ,(intern (string slotname) :keyword)
                                 :col-type ,col-type
                                 ,@other-args))
         (:metaclass dao-class)
         ,@(when-let (keys (assoc :keys dao-options)) `(,keys)))
       (deftable ,name
         (!dao-def)
         ,@(loop for (opt . args) in dao-options
              for form = (case opt
                           (:index `(!index ,@(mapcar (curry #'list 'quote) args)))
                           (:unique-index `(!unique-index ,@(mapcar (curry #'list 'quote) args)))
                           (:foreign-key `(!foreign ,@args))
                           (:unique `(!unique ,@(mapcar (curry #'list 'quote) args)))
                           (:query `(query ,@args))
                           (:keys nil)
                           (otherwise (error "Unknown defdao option: ~S" opt)))
              when form
              collect form)))))

(defmacro with-transaction ((&key name) &body body)
  `(with-db ()
     (pomo:with-transaction (,@(when name `(,name)))
       ,@body)))

(defun insert-row (class-name &rest initargs)
  (with-db () (id (apply #'make-dao class-name initargs))))

;;;
;;; DB booting
;;;
(defun drop-table (symbol)
  (db-query (format nil "drop table if exists ~A" (sql-compile symbol))))
(defun drop-all-tables ()
  (map nil (compose #'drop-table #'car) pomo::*tables*))

(defun rebuild-table (table-name)
  (with-transaction ()
    (drop-table table-name)
    (create-table table-name)))

(defun dblog (format-string &rest format-args)
  (format t "~&LOG - ~A~%" (apply #'format nil format-string format-args)))

(defvar *init-hooks* (make-hash-table))
(defun register-db-init-hook (hook-name function)
  (setf (gethash hook-name *init-hooks*) function))
(defun remove-db-init-hook (name)
  (remhash name *init-hooks*))
(defun run-db-init-hook (name)
  (if-let (func (gethash name *init-hooks*))
    (funcall func)
    (error "No hook named ~A." name)))

(defun rebuild-db ()
  (with-db ()
    (drop-all-tables))
  (init-db))

(defun init-db ()
  (with-db ()
    (create-all-tables))
  (maphash (lambda (hook-name function)
             (dblog "Executing db-boot hook: ~A" hook-name)
             (funcall function))
           *init-hooks*))

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
