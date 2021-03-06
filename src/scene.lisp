(util:def-file-package #:sykosomatic.scene
  (:use :sykosomatic.db)
  (:export :create-scene :add-action :add-dialogue :find-scenes-by-account-id
           :scene-exists-p :account-voted-p
           :find-scenes-by-account-email :find-scene-with-entries :find-scene-entries
           :scene-id :scene-upvote :scene-rating))

(defdao scene ()
  ((account-id bigint))
  (:keys id))
(defun create-scene (account-id)
  (insert-row 'scene :account-id account-id))

(defdao scene-action ()
  ((scene-id bigint)
   (actor-id bigint)
   (action text)
   (timestamp timestamp :col-default (:now)))
  (:keys id))
(defun add-action (scene-id actor-id action-txt)
  (insert-row 'scene-action
              :scene-id scene-id
              :actor-id actor-id
              :action action-txt))

(defdao scene-dialogue ()
  ((scene-id bigint)
   (actor-id bigint)
   (dialogue (or db-null text))
   (parenthetical (or db-null text))
   (timestamp timestamp :col-default (:now)))
  (:keys id))
(defun add-dialogue (scene-id actor-id dialogue parenthetical)
  (insert-row 'scene-dialogue
              :scene-id scene-id
              :actor-id actor-id
              :dialogue (or dialogue :null)
              :parenthetical (or parenthetical :null)))

(defdao scene-upvote ()
  ((scene-id bigint)
   (account-id bigint))
  (:keys id))

(defun scene-upvote (scene-id account-id)
  (with-transaction ()
    (unless (account-voted-p scene-id account-id)
      (insert-row 'scene-upvote :scene-id scene-id :account-id account-id))
    t))

(defun scene-id (scene)
  (id scene))

(defun scene-exists-p (scene-id)
  (db-query (:select t :from 'scene :where (:= 'id scene-id))
            :single))

(defun account-voted-p (scene-id account-id)
  (db-query (:select t :from 'scene-upvote
                     :where (:and (:= 'scene-id scene-id)
                                  (:= 'account-id account-id)))
            :single))

(defun scene-rating (scene-id)
  (db-query (:select (:count :*) :from 'scene-upvote
                     :where (:= 'scene-id scene-id))
            :single))

(defun find-scenes-by-account-id (account-id)
  (with-db ()
    (select-dao 'scene (:= 'account-id account-id))))

(defun find-scenes-by-account-email (account-email)
  (with-transaction ()
    (when-let (account-id (query (:select 'id :from 'account
                                          :where (:= 'email (string-downcase account-email)))
                                 :single))
      (find-scenes-by-account-id account-id))))

(defun find-scene-with-entries (scene-id)
  (db-query (:select :* :from (:as 'scene 's)
                     :left-join (:as 'dialogue 'd)
                     :on (:= 'd.scene-id 's.id)
                     :left-join (:as 'action 'a)
                     :on (:= 'a.scene-id 's.id)
                     :where (:= 's.id scene-id))
            :alists))

(defun find-scene-entries (scene-id)
  (find-scene-with-entries scene-id))
