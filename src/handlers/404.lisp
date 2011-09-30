(util:def-file-package #:sykosomatic.handler.404
  (:use :hunchentoot)
  (:export :404-handler))

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (templ:render-template "not-found"))
