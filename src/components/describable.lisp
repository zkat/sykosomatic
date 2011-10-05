(util:def-file-package #:sykosomatic.components.describable
  (:use :sykosomatic.db
        :sykosomatic.entity)
  (:export :noun
           :adjectives
           :nickname
           :base-description
           :short-description))

(defdao noun ()
  ((entity-id bigint)
   (noun text)))

(defun noun (entity)
  (db-query (:select 'noun :from 'noun :where (:= 'entity-id entity)) :single))
(defun (setf noun) (new-value entity)
  (with-transaction ()
    (cond ((null new-value)
           (db-query (:delete-from 'noun :where (:= 'entity-id entity))))
          ((noun entity)
           (db-query (:update 'noun :set 'noun new-value :where (:= 'entity-id entity))))
          (t
           (insert-row 'noun :entity-id entity :noun new-value))))
  new-value)

(defdao adjective ()
  ((entity-id bigint)
   (adjective text)))

(defun adjectives (entity)
  (db-query (:select 'adjective :from 'adjective :where (:= 'entity-id entity))
            :column))
(defun (setf adjectives) (new-value entity)
  (with-transaction ()
    (db-query (:delete-from 'adjective :where (:= 'entity-id entity)))
    (map nil (lambda (new-adj) (insert-row 'adjective :entity-id entity :adjective new-adj))
         new-value))
  new-value)

(defdao feature ()
  ((entity-id bigint)
   (feature-id bigint)))

(defun base-description (entity)
  (when-let (result (db-query (:select 'n.noun (:array-agg 'a.adjective)
                                       :from (:as 'noun 'n)
                                       :left-join (:as 'adjective 'a)
                                       :on (:= 'a.entity-id 'n.entity-id)
                                       :where (:= 'n.entity-id entity)
                                       :group-by 'noun)
                              :row))
    (destructuring-bind (noun adjectives) result
      (with-output-to-string (s)
        (princ "a " s)
        (when-let (adjs (coerce (remove :null adjectives) 'list))
          (format s (concatenate 'string *english-list-format-string* " ") adjs))
        (princ noun s)))))

(defdao nickname ()
  ((entity-id bigint)
   (observer-id bigint)
   (nickname text)))

(defun nickname (observer entity)
  (db-query (:select 'nickname :from 'nickname
                     :where (:and (:= 'observer-id observer)
                                  (:= 'entity-id entity)))
            :single))

(defun (setf nickname) (new-value observer entity)
  (with-transaction ()
    (cond ((null new-value)
           (db-query (:delete-from 'nickname :where (:and
                                                     (:= 'observer-id observer)
                                                     (:= 'entity-id entity)))))
          ((nickname observer entity)
           (db-query (:update 'nickname :set 'nickname new-value
                              :where (:and
                                      (:= 'observer-id observer)
                                      (:= 'entity-id entity)))))
          (t
           (insert-row 'nickname
                       :entity-id entity
                       :observer-id observer
                       :nickname new-value))))
  new-value)

(defun all-nicknames (entity)
  (db-query (:select 'nickname 'observer-id :from 'nickname
                     :where (:= 'entity-id entity))
            :plists))

(defun short-description (observer entity)
  (or (nickname observer entity)
      (base-description entity)))
