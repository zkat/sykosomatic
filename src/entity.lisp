(cl:defpackage #:sykosomatic.entity
  (:use :cl :alexandria :postmodern :sykosomatic.db :sykosomatic.util)
  (:export :init-entity-system :teardown-entity-system
           :list-systems :register-system :unregister-system
           :list-modifiers :add-modifier :create-entity
           :modifier-value :multiple-modifier-values :entity-uid
           :find-by-modifier-value :find-entity-by-uid
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
   (package :col-type text :initarg :package)
   (name :col-type text :initarg :name)
   (description :col-type (or db-null text) :initarg :description)
   ;; NOTE: If another value type is added here, update:
   ;; ADD-MODIFIER, MODIFIER-VALUE, FIND-BY-MODIFIER-VALUE.
   (numeric-value :col-type (or db-null numeric) :initarg :numeric-value)
   (boolean-value :col-type (or db-null boolean) :initarg :boolean-value)
   (text-value :col-type (or db-null text) :initarg :text-value)
   (text-array-value :col-type (or db-null text[]) :initarg :text-array-value))
  (:keys id)
  (:index package name id entity-id))

(defun entity-id (entity)
  ;; Just numbers for now.
  entity)

(defun list-modifiers (entity &optional (package *package*))
  (with-db ()
    (query (sql-compile `(:select :* :from 'modifier
                                  :where (:and (:= 'entity-id ,(entity-id entity))
                                               ,@(when package
                                                   `((:= 'package ,(etypecase package
                                                                    (package (package-name package))
                                                                    (string package)
                                                                    (symbol (string package)))))))))
           :alists)))

(defun add-modifier (entity name value &key
                     (precedence 0) (description :null))
  (assert (symbolp name) (name) "Modifier names must be symbols. Got ~S instead." name)
  (with-db ()
    (apply #'make-dao 'modifier
           :entity-id (entity-id entity)
           :package (package-name (symbol-package name))
           :name (symbol-name name)
           :description description
           :precedence precedence
           (etypecase value
             (number (list :numeric-value value))
             (string (list :text-value value))
             (vector (list :text-array-value value))
             (boolean (list :boolean-value value))
             (symbol (list :text-value (princ-to-string value)))))))

(defun delete-modifier (modifier-id)
  (with-db ()
    (query (:delete-from 'modifier :where (:= 'id modifier-id)))))

(defun modifier-value (entity name)
  (with-db ()
    (find-if-not (curry #'eq :null)
                 (query (:order-by (:select 'text-value 'numeric-value
                                            'boolean-value 'text-array-value
                                            :from 'modifier
                                            :where (:and (:= 'entity-id (entity-id entity))
                                                         (:= 'package (package-name (symbol-package name)))
                                                         (:= 'name (symbol-name name))))
                                   (:desc 'precedence)
                                   (:desc 'id))
                        :row))))

(defun (setf modifier-value) (new-value entity name column)
  (with-db ()
    (query (:update 'modifier
                    :set column new-value
                    :where (:and (:= 'entity-id (entity-id entity))
                                 (:= 'package (package-name (symbol-package name)))
                                 (:= 'name (symbol-name name)))))))

(defun create-entity (&key comment)
  (id (with-db () (make-dao 'entity :comment (or comment :null)))))

(defun entity-uid (entity)
  (with-db () (modifier-value entity 'uid)))

(defun (setf entity-uid) (new-value entity)
  (with-db ()
    (with-transaction ()
      (cond ((entity-uid entity)
             (setf (modifier-value entity 'uid 'text-value) new-value))
            ((find-entity-by-uid new-value)
             (error "~S must be a globally unique identifier, but it already identifies entity ~A."
                    new-value (find-entity-by-uid new-value)))
            (t
             (add-modifier entity 'uid new-value
                           :description "Unique external identifier for entity."))))))

(defun find-by-modifier-value (modifier-name value &key (test :=) (allp nil))
  (with-db ()
    (let ((q (sql-compile
              `(:order-by
                (:select 'entity-id :from 'modifier
                         :where (:and (:= 'package ,(package-name (symbol-package modifier-name)))
                                      (:= 'name ,(symbol-name modifier-name))
                                      (,test ,(if (stringp value)
                                                  `(:unaccent ,value)
                                                  value)
                                             ,(etypecase value
                                                         (number 'numeric-value)
                                                         (string '(:unaccent text-value))
                                                         (vector 'text-array-value)
                                                         (boolean 'boolean-value)
                                                         (symbol 'text-value)))))
                (:desc 'precedence)))))
      (if allp
          (query q :column)
          (query q :single)))))

(defun find-entity-by-uid (uid)
  (find-by-modifier-value 'uid uid))

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
