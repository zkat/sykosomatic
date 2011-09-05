(cl:defpackage #:sykosomatic.entity
  (:use :cl :postmodern :sykosomatic.db))
(cl:in-package #:sykosomatic.entity)

(defdao entity ()
  ((id :col-type serial :reader id)
   (comment :col-type text))
  (:keys id))

(defdao modifier ()
  ((id :col-type serial :reader id)
   (entity-id :col-type bigint :initarg :entity-id)
   (precedence :col-type bigint :initarg :precedence :col-default 0)
   (type :col-type text :initarg :type)
   (description :col-type (or db-null text) :initarg :description)
   (numeric-value :col-type (or db-null numeric) :initarg :numeric-value)
   (text-value :col-type (or db-null text) :initarg :text-value)
   (timestamp-value :col-type (or db-null timestamp) :initarg :timestamp-value))
  (:keys id))

(defun entity-id (entity)
  ;; Just numbers for now.
  entity)

(defun list-modifiers (entity)
  (query (:select :* :from 'modifier :where (:= 'entity-id (entity-id entity)))
         :alists))

(defun add-modifier (entity type &key
                     text-value numeric-value timestamp-value
                     precedence description)
  (with-transaction ()
    (make-dao 'modifier
              :entity-id (entity-id entity)
              :type type :text-value (or text-value :null)
              :numeric-value (or numeric-value :null)
              :timestamp-value (or timestamp-value :null)
              :description (or description :null)
              :precedence (or precedence 0))))
