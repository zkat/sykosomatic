(cl:defpackage #:sykosomatic.command
  (:use :cl :alexandria
        :sykosomatic.util
        :sykosomatic.vocabulary
        :sykosomatic.db)
  (:export :*actor*
           :*verb*
           :*adverbs*
           :*direct-object*
           :*indirect-object*
           :*direct-preposition*
           :*indirect-preposition*
           :defcommand
           :add-verb-command
           :verb-command
           :remove-verb-command
           :remove-command))
(cl:in-package #:sykosomatic.command)

;;; Command vars
(defvar *actor*)
(defvar *verb*)
(defvar *adverbs*)
(defvar *direct-object*)
(defvar *indirect-object*)
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

(defmacro defcommand (name &body body)
  `(ensure-command ,(string name)
                   (lambda ()
                     ,@body)))

;;; Link to verbs
(defdao verb-command ()
  ((verb-id bigint)
   (command-name text)))

(defun verb-command (verb)
  (let ((command-name
         (db-query (:select 'c.command-name :from (:as 'verb-command 'c)
                            :inner-join (:as 'verb 'v)
                            :on (:= 'v.id 'c.verb-id)
                            :where (:= 'v.third-person verb))
                   :single)))
    (find-command command-name)))
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
