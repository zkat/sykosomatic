(cl:defpackage #:sykosomatic.utils
  (:use :cl)
  (:export :logit :dbg))
(cl:in-package :sykosomatic.utils)

(defun logit (format-string &rest format-args)
  (format t "~&~A~%" (apply #'format nil format-string format-args)))

(defun dbg (comment obj)
  (format t "DBG XXX: ~A (~S)" comment obj)
  obj)