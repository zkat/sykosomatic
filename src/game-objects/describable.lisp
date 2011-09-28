(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria
         :sykosomatic.util
         :sykosomatic.db
         :sykosomatic.entity))
(cl:in-package #:sykosomatic.game-objects.describable)

(defun short-description (entity)
  (modifier-value entity 'short-description))

(defun long-description (entity)
  (modifier-value entity 'long-description))
