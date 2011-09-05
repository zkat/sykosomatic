(cl:defpackage #:sykosomatic.entity
  (:use :cl :alexandria :postmodern :sykosomatic.db :sykosomatic.utils)
  (:export :init-entity-system :teardown-entity-system
           :list-systems :register-system :unregister-system
           :list-modifiers :add-modifier :create-entity
           :text-modifier-value :numeric-modifier-value
           :entity-uid :find-entity-by-uid
           :event-execution :expire-modifier
           :clear-expired-modifiers))
(cl:in-package #:sykosomatic.entity)

(optimizations)

(let ((callbacks (make-hash-table)))
  (defun list-systems ()
    (hash-table-keys callbacks))
  (defun register-system (system-name callback)
    (setf (gethash system-name callbacks) callback))
  (defun unregister-system (system-name)
    (remhash system-name callbacks))
  (defun execute-all-callbacks ()
    (maphash-values #'funcall callbacks)))

(defvar *es-thread* nil)
(defun init-entity-system ()
  (setf *es-thread*
        (bt:make-thread (lambda ()
                          ;; TODO - clamp this.
                          (loop (execute-all-callbacks)))
                        :name "entity-system-processing")))

(defun teardown-entity-system ()
  (when (and *es-thread* (bt:thread-alive-p *es-thread*))
    (bt:destroy-thread *es-thread*))
  (setf *es-thread* nil))

(defdao entity ()
  ((id :col-type serial :reader id)
   (comment :col-type (or db-null text)))
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

(defun text-modifier-value (entity type)
  (query (:order-by (:select 'text-value :from 'modifier
                             :where (:and (:= 'entity-id (entity-id entity))
                                          (:= 'type type)))
                    (:desc 'precedence))
         :single))

(defun numeric-modifier-value (entity type)
  (query (:order-by (:select 'numeric-value :from 'modifier
                             :where (:and (:= 'entity-id (entity-id entity))
                                          (:= 'type type)))
                    (:desc 'precedence))
         :single))

(defun create-entity (&key comment)
  (id (make-dao 'entity :comment (or comment :null))))

(defun entity-uid (entity)
  (query (:select 'text-value :from 'modifier
                  :where (:and (:= 'entity-id (entity-id entity))
                               (:= 'type "entity-uid")))
         :single))

(defun (setf entity-uid) (new-value entity)
  (with-transaction ()
    (if (entity-uid entity)
        (query (:update 'modifier
                        :set 'text-value new-value
                        :where (:and (:= 'entity-id (entity-id entity))
                                     (:= 'type "entity-uid"))))
        (progn
          (assert (not (query (:select t :from 'modifier
                                       :where (:and (:= 'type "entity-uid")
                                                    (:= 'text-value new-value)))
                              :single))
                  () "~S is not a globally unique entity identifier." new-value)
          (add-modifier entity "entity-uid" :text-value new-value :description
                        "Unique human-usable identifier for entity.")))))

(defun find-entity-by-uid (uid)
  (query (:select 'entity-id :from 'modifier
                  :where (:and (:= 'type "entity-uid")
                               (:= 'text-value uid)))
         :single))

;;; Events
(defdao event-execution ()
  ((id :col-type serial :reader id)
   (event-id :col-type bigint :initarg :event-id)
   (type :col-type text :initarg :type)
   (execution-time :col-type timestamp :col-default (:now) :initarg :execution-time)
   (completedp :col-type boolean :col-default nil))
  (:keys id))

(defdao ev-remove-modifier ()
  ((id :col-type serial :reader id)
   (modifier-id :col-type bigint :initarg :modifier-id))
  (:keys id))

(defprepared expired-modifiers
    (:select 'ex.id 'mod.id :from (:as 'event-execution 'ex)
             :inner-join (:as 'ev-remove-modifier 'ev)
             :on (:and (:= 'ex.type "remove-modifier")
                       (:= 'ex.event-id 'ev.id))
             :inner-join (:as 'modifier 'mod)
             :on (:= 'ev.modifier-id 'mod.id)
             :where (:and (:<= 'ex.execution-time (:now))
                          (:not 'ex.completedp))))

(defun expire-modifier (modifier-id &key (expiration (get-universal-time)))
  (with-transaction ()
    (let ((removal (make-dao 'ev-remove-modifier :modifier-id modifier-id)))
      (make-dao 'event-execution :type "remove-modifier"
                :event-id (id removal)
                :execution-time (local-time:to-rfc3339-timestring
                                 (local-time:universal-to-timestamp expiration))))))

(defun clear-expired-modifiers ()
  (flet ((process-penalty (p)
           (destructuring-bind (event-execution-id modifier-id)
               p
             (dblog "Removing modifier ~A" modifier-id)
             (query (:delete-from 'modifier :where (:= 'id modifier-id)))
             (query (:update 'event-execution :set 'completedp t
                             :where (:= 'id event-execution-id))))))
    (with-transaction ()
      (map nil #'process-penalty (expired-modifiers)))))

(register-system 'modifier-expiration 'clear-expired-modifiers)
