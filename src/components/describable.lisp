(util:def-file-package #:sykosomatic.components.describable
  (:use :sykosomatic.db
   :sykosomatic.entity))

(defun short-description (entity)
  (modifier-value entity 'short-description))

(defun long-description (entity)
  (modifier-value entity 'long-description))
