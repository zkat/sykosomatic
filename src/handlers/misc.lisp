(cl:defpackage #:sykosomatic.handler.misc
  (:use :cl :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session)
  (:export :ajax-ping))
(cl:in-package #:sykosomatic.handler.misc)

(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in)
  (setf (content-type*) "text/plain")
  "pong")
