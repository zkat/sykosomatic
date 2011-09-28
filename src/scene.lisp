(cl:defpackage #:sykosomatic.scene
  (:use :cl :alexandria
        :sykosomatic.util
        :sykosomatic.db)
  (:export :create-scene :add-action :add-dialogue :find-scenes-by-account-id
           :scene-exists-p :account-voted-p
           :find-scenes-by-account-email :find-scene-with-entries :find-scene-entries
           :scene-id :scene-upvote :scene-rating))
(cl:in-package #:sykosomatic.scene)

(defdao scene ()
  ((id :col-type serial :reader id)
   (account-id :col-type bigint :initarg :account-id))
  (:keys id))
(defun create-scene (account-id)
  (with-db ()
    (make-dao 'scene :account-id account-id)))

(defdao scene-action ()
  ((id :col-type serial :reader id)
   (scene-id :col-type bigint :initarg :scene-id)
   (actor-id :col-type bigint :initarg :actor-id)
   (action :col-type text :initarg :action)
   (timestamp :col-type timestamp :col-default (:now) :initarg :timestamp))
  (:keys id))
(defun add-action (scene-id actor-id action-txt)
  (with-db ()
    (make-dao 'scene-action
              :scene-id scene-id
              :actor-id actor-id
              :action action-txt)))

(defdao scene-dialogue ()
  ((id :col-type serial :reader id)
   (scene-id :col-type bigint :initarg :scene-id)
   (actor-id :col-type bigint :initarg :actor-id)
   (dialogue :col-type (or db-null text) :initarg :dialogue)
   (parenthetical :col-type (or db-null text) :initarg :parenthetical)
   (timestamp :col-type timestamp :col-default (:now) :initarg :timestamp))
  (:keys id))
(defun add-dialogue (scene-id actor-id dialogue parenthetical)
  (with-db ()
    (make-dao 'scene-dialogue
              :scene-id scene-id
              :actor-id actor-id
              :dialogue (or dialogue :null)
              :parenthetical (or parenthetical :null))))

(defdao scene-upvote ()
  ((id :col-type serial :reader id)
   (scene-id :col-type bigint :initarg :scene-id)
   (account-id :col-type bigint :initarg :account-id))
  (:keys id))

(defun scene-upvote (scene-id account-id)
  (with-db ()
    (with-transaction ()
      (unless (account-voted-p scene-id account-id)
        (make-dao 'scene-upvote))
      t)))

(defun scene-id (scene)
  (id scene))

(defun scene-exists-p (scene-id)
  (with-db ()
    (query (:select t :from 'scene :where (:= 'id scene-id))
           :single)))

(defun account-voted-p (scene-id account-id)
  (with-db ()
    (query (:select t :from 'scene-upvote
                    :where (:and (:= 'scene-id scene-id)
                                 (:= 'account-id account-id)))
           :single)))

(defun scene-rating (scene-id)
  (with-db ()
    (query (:select (:count :*) :from 'scene-upvote
                    :where (:= 'scene-id scene-id)))))

(defun find-scenes-by-account-id (account-id)
  (with-db ()
    (select-dao 'scene (:= 'account-id account-id))))

(defun find-scenes-by-account-email (account-email)
  (with-db ()
    (with-transaction ()
      (when-let (account-id (query (:select 'id :from 'account
                                            :where (:= 'email (string-downcase account-email)))
                                   :single))
        (find-scenes-by-account-id account-id)))))

(defun find-scene-with-entries (scene-id)
  (with-db ()
    (query (:select :* :from (:as 'scene 's)
                    :left-join (:as 'dialogue 'd)
                    :on (:= 'd.scene-id 's.id)
                    :left-join (:as 'action 'a)
                    :on (:= 'a.scene-id 's.id)
                    :where (:= 's.id scene-id))
           :alists)))

(defun find-scene-entries (scene-id)
  (find-scene-with-entries scene-id))
