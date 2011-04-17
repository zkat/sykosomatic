(cl:defpackage #:sykosomatic.utils
  (:use :cl)
  (:export :logit))
(cl:in-package :sykosomatic.utils)

(defun logit (format-string &rest format-args)
  (format t "~&~A~%" (apply #'format nil format-string format-args)))