(util:def-file-package #:sykosomatic.command
  (:use :sykosomatic.db
        :sykosomatic.vocabulary)
  (:export :*actor*
           :*verb*
           :*adverb*
           :*adverb-position*
           :*direct-objects*
           :*indirect-objects*
           :*preposition*
           :defcommand
           :add-verb-command
           :invoke-verb-command
           :remove-verb-command
           :remove-command
           :get-all-messages
           :tell
           :tell-local))

;;; Command vars
(defvar *actor*)
(defvar *verb*)
(defvar *adverb*)
(defvar *adverb-position*)
(defvar *direct-objects*)
(defvar *indirect-objects*)
(defvar *preposition*)

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
                                       :where (:= 'v.bare (verb-bare verb)))
                              :single))
      (error "No command associated with verb: ~S" verb)))

(defun add-verb-command (verb command-name)
  (with-transaction ()
    (if-let (verb-id (db-query (:select 'id :from 'verb :where (:= 'bare verb))
                               :single))
      (id (make-dao 'verb-command :verb-id verb-id :command-name (string command-name)))
      (error "Unknown verb: ~S" verb))))
(defun remove-verb-command (verb)
  (with-transaction ()
    (if-let (verb-id (db-query (:select 'id :from 'verb :where (:= 'bare verb))
                               :single))
      (db-query (:delete-from 'verb-command :where (:= 'verb-id verb-id)))
      (error "Unknown verb: ~S" verb))))

(defun invoke-verb-command (&key
                            ((:actor *actor*)) ((:verb *verb*))
                            ((:adverb *adverb*)) ((:adverb-position *adverb-position*))
                            ((:direct-objects *direct-objects*))
                            ((:indirect-objects *indirect-objects*))
                            ((:preposition *preposition*)))
  (funcall (verb-command *verb*)))

;;; Command-writing utilities
(defvar *tell-lock* (bt:make-lock))
(defvar *messages* nil)
(defun send-to-entity (entity message)
  (bt:with-lock-held (*tell-lock*)
    (push (cons entity message) *messages*)))
(defun get-all-messages ()
  (bt:with-lock-held (*tell-lock*)
    (prog1 (nreverse *messages*)
      (setf *messages* nil))))

(defun tell (entity format-string &rest format-args
             &aux (message (apply #'format nil format-string format-args)))
  "Sends a message to a specific entity."
  (send-to-entity entity message))
(defun tell-local (entity format-string &rest format-args
                   &aux (message (apply #'format nil format-string format-args)))
  "Sends a message to all local entities."
  ;; Right now, just sends it to all existing entities.
  (declare (ignore entity))
  (map nil (rcurry #'send-to-entity message)
       (db-query (:select 'id :from 'entity) :column)))
