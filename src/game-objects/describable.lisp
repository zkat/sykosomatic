(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria :sykosomatic.entity :sykosomatic.db :sykosomatic.util :postmodern))
(cl:in-package #:sykosomatic.game-objects.describable)

(defun short-name (entity)
  (modifier-value entity "desc" "short-name" :text))

(defun full-name (entity)
  (with-db ()
    (when-let (short-name (short-name entity))
      (let ((article (modifier-value entity "desc" "article" :text))
            (adjectives (query (:select 'text-value :from 'modifier
                                        :where (:and (:= 'entity-id entity)
                                                     (:= 'ns "desc")
                                                     (:= 'name "adjective")))
                               :column)))
        (format nil "~@[~A ~]~{~A ~^~}~A" article adjectives short-name)))))

(defun short-description (entity)
  (modifier-value entity "desc" "short-description" :text))

(defun long-description (entity)
  (modifier-value entity "desc" "long-description" :text))
