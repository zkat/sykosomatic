(util:def-file-package #:sykosomatic.handler.signup
  (:use :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.account
        :sykosomatic.util.form)
  (:export :signup))

(define-easy-handler (signup :uri "/signup") ()
  (case (request-method*)
    (:post
     (let ((form (make-form 'signup (post-parameters*))))
       (cond ((form-valid-p form)
              (create-account form)
              (logit "Account created: ~A" (field-value form :email))
              (push-error "Please log in.")
              (redirect "/login"))
             (t
              (templ:render-template "signup" form)))))
    (:get
     (with-form-errors
       (templ:render-template "signup" (make-form 'signup))))))
