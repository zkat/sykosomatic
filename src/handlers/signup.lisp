(cl:defpackage #:sykosomatic.handler.signup
  (:use :cl :hunchentoot
        :alexandria
        :sykosomatic.util
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.account)
  (:export :signup))
(cl:in-package #:sykosomatic.handler.signup)

(define-easy-handler (signup :uri "/signup") (email display-name password confirmation)
  (case (request-method*)
    (:post
     (multiple-value-bind (account-created-p errors)
         (create-account email display-name password confirmation)
       (if account-created-p
           (progn
             (logit "Account created: ~A" email)
             (push-error "Please log in.")
             (redirect "/login"))
           (progn
             (appendf (session-errors) errors)
             (redirect "/signup")))))
    (:get
     (with-form-errors
       (templ:render-template "signup")))))