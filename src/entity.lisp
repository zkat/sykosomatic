(cl:defpackage #:sykosomatic.entity
  (:use :cl :alexandria :postmodern :sykosomatic.db :sykosomatic.util)
  (:export :init-entity-system :teardown-entity-system
           :list-systems :register-system :unregister-system
           :list-modifiers :add-modifier :create-entity
           :modifier-value :entity-uid :find-entity-by-uid
           :event-execution :expire-modifier
           :clear-expired-modifiers))
(cl:in-package #:sykosomatic.entity)

(let ((callbacks (make-hash-table)))
  (defun list-systems ()
    (hash-table-keys callbacks))
  (defun register-system (system-name callback)
    (setf (gethash system-name callbacks) callback))
  (defun unregister-system (system-name)
    (remhash system-name callbacks))
  (defun execute-all-callbacks ()
    (maphash-values #'funcall callbacks)))

(defparameter *ticks-per-second* 5)
(defvar *entity-system-thread* nil)
(defun init-entity-system ()
  (setf *entity-system-thread*
        (bt:make-thread (lambda ()
                          (loop with timer = (make-timer *ticks-per-second*)
                             do (timer-tick timer)
                               (continuable (execute-all-callbacks))))
                        :name "entity-system-processing")))

(defun teardown-entity-system ()
  (when (and *entity-system-thread* (bt:thread-alive-p *entity-system-thread*))
    (bt:destroy-thread *entity-system-thread*))
  (setf *entity-system-thread* nil))

(defdao entity ()
  ((id :col-type serial :reader id)
   (comment :col-type (or db-null text)))
  (:keys id))

(defdao modifier ()
  ((id :col-type serial :reader id)
   (entity-id :col-type bigint :initarg :entity-id)
   (precedence :col-type bigint :initarg :precedence :col-default 0)
   (ns :col-type text :initarg :ns)
   (name :col-type text :initarg :name)
   (description :col-type (or db-null text) :initarg :description)
   ;; NOTE: If another value type is added here, ADD-MODIFIER and MODIFIER-VALUE must be amended.
   (numeric-value :col-type (or db-null numeric) :initarg :numeric-value)
   (text-value :col-type (or db-null text) :initarg :text-value))
  (:keys id))

(defun entity-id (entity)
  ;; Just numbers for now.
  entity)

(defun list-modifiers (entity &optional ns name)
  (with-db ()
    (query (sql-compile `(:select :* :from 'modifier
                                  :where (:and (:= 'entity-id ,(entity-id entity))
                                               ,@(when ns `((:= 'ns ,ns)))
                                               ,@(when name `((:= 'name ,name))))))
           :alists)))

(defun add-modifier (entity ns name value &key
                     precedence description)
  (with-db ()
    (apply #'make-dao 'modifier
           :entity-id (entity-id entity)
           :ns ns :name name
           :description (or description :null)
           :precedence (or precedence 0)
           (typecase value
             (number (list :numeric-value value))
             (otherwise (list :text-value (princ-to-string value)))))))

(defun delete-modifier (modifier-id)
  (with-db ()
    (query (:delete-from 'modifier :where (:= 'id modifier-id)))))

(defun modifier-value (entity ns name)
  (with-db ()
    (find-if-not (curry #'eq :null)
                 (query (:limit
                         (:order-by (:select 'text-value 'numeric-value
                                             :from 'modifier
                                             :where (:and (:= 'entity-id (entity-id entity))
                                                          (:= 'ns ns)
                                                          (:= 'name name)))
                                    (:desc 'precedence)
                                    (:desc 'id))
                         1)
                        :row))))

(defun create-entity (&key comment)
  (id (with-db () (make-dao 'entity :comment (or comment :null)))))

(defun entity-uid (entity)
  (with-db () (modifier-value entity "entity" "uid")))

(defun (setf entity-uid) (new-value entity)
  (with-db ()
    (with-transaction ()
      (cond ((entity-uid entity)
             (query (:update 'modifier
                          :set 'text-value new-value
                          :where (:and (:= 'entity-id (entity-id entity))
                                       (:= 'ns "entity")
                                       (:= 'name "uid")))))
            ((find-entity-by-uid new-value)
             (error "~S must be a globally unique identifier, but it already identifies entity ~A."
                    new-value (find-entity-by-uid new-value)))
            (t
             (add-modifier entity "entity" "uid" new-value
                           :description "Unique external identifier for entity."))))))

(defun find-entity-by-uid (uid)
  (with-db ()
    (query (:select 'entity-id :from 'modifier
                    :where (:and (:= 'ns "entity")
                                 (:= 'name "uid")
                                 (:= 'text-value uid)))
           :single)))

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
  (with-db ()
    (with-transaction ()
      (let ((removal (make-dao 'ev-remove-modifier :modifier-id modifier-id)))
        (make-dao 'event-execution :type "remove-modifier"
                  :event-id (id removal)
                  :execution-time (local-time:to-rfc3339-timestring
                                   (local-time:universal-to-timestamp expiration)))))))

(defun clear-expired-modifiers ()
  (with-db ()
    (flet ((process-penalty (p)
             (destructuring-bind (event-execution-id modifier-id)
                 p
               (query (:delete-from 'modifier :where (:= 'id modifier-id)))
               (query (:update 'event-execution :set 'completedp t
                               :where (:= 'id event-execution-id))))))
      (with-transaction ()
        (map nil #'process-penalty (expired-modifiers))))))

(register-system 'modifier-expiration 'clear-expired-modifiers)
