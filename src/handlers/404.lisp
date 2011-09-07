(cl:defpackage #:sykosomatic.handler.404
  (:use :cl :hunchentoot)
  (:export :404-handler))
(cl:in-package #:sykosomatic.handler.404)

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (templ:render-template "not-found"))
