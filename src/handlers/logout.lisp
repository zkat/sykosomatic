(cl:defpackage #:sykosomatic.handler.logout
  (:use :cl :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session)
  (:export :logout))
(cl:in-package #:sykosomatic.handler.logout)

(define-easy-handler (logout-page :uri "/logout") ()
  (when *session*
    (end-session *session*))
  (redirect "/login"))
