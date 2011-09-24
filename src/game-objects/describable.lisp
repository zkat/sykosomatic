(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria :sykosomatic.entity :sykosomatic.db :sykosomatic.util :postmodern))
(cl:in-package #:sykosomatic.game-objects.describable)

(defun short-name (entity)
  (modifier-value entity 'short-name))

(defun full-name (entity)
  (with-db ()
    (when-let (short-name (short-name entity))
      (let ((article (modifier-value entity 'article))
            (adjectives (modifier-value entity 'adjectives)))
        (format nil "~@[~A ~]~{~A ~^~}~A" article (coerce adjectives 'list) short-name)))))

(defun short-description (entity)
  (modifier-value entity 'short-description))

(defun long-description (entity)
  (modifier-value entity 'long-description))
