(util:def-file-package #:sykosomatic.handler.logout
  (:use :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session)
  (:export :logout))

(define-easy-handler (logout-page :uri "/logout") ()
  (when *session*
    (end-session *session*))
  (redirect "/"))
