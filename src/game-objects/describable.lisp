(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria :sykosomatic.entity :sykosomatic.db :sykosomatic.util :postmodern))
(cl:in-package #:sykosomatic.game-objects.describable)

(optimizations)

(defun short-name (entity)
  (text-modifier-value entity "desc:short-name"))

(defun full-name (entity)
  (with-db ()
    (when-let (short-name (short-name entity))
      (let ((article (text-modifier-value entity "desc:article"))
            (adjectives (query (:select 'text-value :from 'modifier
                                        :where (:and (:= 'entity-id entity)
                                                     (:= 'type "desc:adjective")))
                               :column)))
        (format nil "~@[~A ~]~{~A ~^~}~A" article adjectives short-name)))))

(defun short-description (entity)
  (text-modifier-value entity "desc:short-description"))

(defun long-description (entity)
  (text-modifier-value entity "desc:long-description"))
