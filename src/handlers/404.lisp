(util:def-file-package #:sykosomatic.handler.404
  (:use :hunchentoot
        :sykosomatic.handler)
  (:export :404-handler))

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (render-page "not-found.html" nil)
  #+nil
  (templ:render-template "not-found"))
