(cl:defpackage #:sykosomatic.entity
  (:use :cl :alexandria
        :sykosomatic.util
        :sykosomatic.db)
  (:export :init-entity-system :teardown-entity-system
           :list-systems :register-system :unregister-system
           :list-modifiers :add-modifier :create-entity
           :modifier-value :multiple-modifier-values :entity-oid
           :find-by-modifier-value :find-entity-by-oid
           :event-execution :expire-modifier
           :clear-expired-modifiers))
(cl:in-package #:sykosomatic.entity)

;;;
;;; Entity System management
;;;
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

;;;
;;; Entity
;;;
(defdao entity ()
  ((comment (or db-null text)))
  (:keys id))

(defun entity-id (entity)
  ;; Just numbers for now.
  entity)

(defun create-entity (&key comment)
  (id (with-db () (make-dao 'entity :comment (or comment :null)))))

;;;
;;; Modifiers
;;;
(defdao modifier ()
  ((entity-id bigint)
   (precedence bigint :col-default 0)
   (package text)
   (name text)
   (description (or db-null text))
   ;; NOTE: If another value type is added here, update:
   ;; ADD-MODIFIER, MODIFIER-VALUE, FIND-BY-MODIFIER-VALUE.
   (numeric-value (or db-null numeric))
   (boolean-value (or db-null boolean))
   (text-value (or db-null text))
   (text-array-value (or db-null text[])))
  (:keys id)
  (:index package name id entity-id))

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

;;;
;;; Entity OIDs
;;;
;;; - These are meant to be unique, human-readable identifiers. Sort of like global variables.
;;;   Example: (setf (entity-oid e) "example-objects:chair")
;;;
(defun entity-oid (entity)
  (with-db () (modifier-value entity 'oid)))

(defun (setf entity-oid) (new-value entity)
  (with-db ()
    (with-transaction ()
      (cond ((entity-oid entity)
             (setf (modifier-value entity 'oid 'text-value) new-value))
            ((find-entity-by-oid new-value)
             (error "~S must be a globally unique identifier, but it already identifies entity ~A."
                    new-value (find-entity-by-oid new-value)))
            (t
             (add-modifier entity 'oid new-value
                           :description "Unique external identifier for entity."))))))

(defun find-entity-by-oid (oid)
  (find-by-modifier-value 'oid oid))

;;;
;;; Events
;;;
(defdao event-execution ()
  ((event-id bigint)
   (type text)
   (execution-time timestamp :col-default (:now))
   (completedp boolean :col-default nil))
  (:keys id))

;;;
;;; Modifier removal events
;;;
(defdao ev-remove-modifier ()
  ((modifier-id bigint))
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
