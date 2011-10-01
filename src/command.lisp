(util:def-file-package #:sykosomatic.command
  (:use :sykosomatic.db
        :sykosomatic.vocabulary)
  (:export :*actor*
           :*verb*
           :*adverbs*
           :*direct-object*
           :*indirect-object*
           :*direct-preposition*
           :*indirect-preposition*
           :defcommand
           :add-verb-command
           :invoke-verb-command
           :remove-verb-command
           :remove-command
           :tell
           :tell-local))

;;; Command vars
(defvar *actor*)
(defvar *verb*)
(defvar *adverbs*)
(defvar *direct-objects*)
(defvar *indirect-objects*)
(defvar *direct-preposition*)
(defvar *indirect-preposition*)

;;; Command definition
(defvar *commands* (make-hash-table :test #'equal))
(defun find-command (name)
  (values (gethash (string name) *commands*)))
(defun ensure-command (name function)
  (when (find-command name)
    (warn "Redefining command ~A." name))
  (setf (gethash (string name) *commands*) function))
(defun remove-command (name)
  (remhash (string name) *commands*))

(defmacro defcommand (name lambda-list &body body)
  (declare (ignore lambda-list))
  `(ensure-command ,(string name)
                   (lambda ()
                     ,@body)))

;;; Link to verbs
(defdao verb-command ()
  ((verb-id bigint)
   (command-name text)))

(defun verb-command (verb)
  (or (find-command (db-query (:select 'c.command-name :from (:as 'verb-command 'c)
                                       :inner-join (:as 'verb 'v)
                                       :on (:= 'v.id 'c.verb-id)
                                       :where (:= 'v.third-person verb))
                              :single))
      (error "No command associated with verb: ~S" verb)))

(defun add-verb-command (verb command-name)
  (with-transaction ()
    (if-let (verb-id (db-query (:select 'id :from 'verb :where (:= 'third-person verb))
                               :single))
      (id (make-dao 'verb-command :verb-id verb-id :command-name (string command-name)))
      (error "Unknown verb: ~S" verb))))
(defun remove-verb-command (verb)
  (with-transaction ()
    (if-let (verb-id (db-query (:select 'id :from 'verb :where (:= 'third-person verb))
                               :single))
      (db-query (:delete-from 'verb-command :where (:= 'verb-id verb-id)))
      (error "Unknown verb: ~S" verb))))

(defun invoke-verb-command (&key
                            ((:actor *actor*)) ((:verb *verb*)) ((:adverbs *adverbs*)) 
                            ((:direct-objects *direct-objects*))
                            ((:indirect-objects *indirect-objects*))
                            ((:direct-preposition *direct-preposition*))
                            ((:indirect-preposition *indirect-preposition*)))
  (funcall (verb-command *verb*)))

;;; Command-writing utilities
(defun tell (entity format-string &rest format-args)
  "Sends a message to a specific entity."
  (logit "Telling entity #~A: ~S"
         entity (apply #'format nil format-string format-args)))
(defun tell-local (entity format-string &rest format-args)
  "Sends a message to all local entities."
  (logit "Telling all entities local to entity #~A: ~S"
         entity (apply #'format nil format-string format-args)))
