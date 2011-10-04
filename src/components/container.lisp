(util:def-file-package :sykosomatic.components.container
  (:use :sykosomatic.db
        :sykosomatic.entity)
  (:export :container-contents
           :entity-container
           :remove-from-container
           :move-to-container
           :others-in-container))

(defdao container-content ()
  ((container-id bigint)
   (content-id bigint)))

(defun container-contents (container)
  (db-query (:select 'content-id :from 'container-content
                     :where (:= 'container-id container))
            :column))

(defun entity-container (entity)
  (db-query (:select 'container-id :from 'container-content
                     :where (:= 'content-id entity))
            :single))

(defun remove-from-container (entity)
  (db-query (:delete-from 'container-content :where (:= 'content-id entity))))

(defun move-to-container (entity target-container)
  (with-transaction ()
    (remove-from-container entity)
    (make-dao 'container-content
              :content-id entity
              :container-id target-container)))

(defun others-in-container (entity)
  (db-query (:select 'c1.content-id :from (:as 'container-content 'c1)
                     :inner-join (:as 'container-content 'c2)
                     :on (:= 'c1.container-id 'c2.container-id)
                     :where (:and
                             (:not (:= 'c1.content-id entity))
                             (:= 'c2.content-id entity)))
            :column))
