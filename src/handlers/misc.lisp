(util:def-file-package #:sykosomatic.handler.misc
  (:use :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session)
  (:export :ajax-ping))

(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in)
  (setf (content-type*) "text/plain")
  "pong")
