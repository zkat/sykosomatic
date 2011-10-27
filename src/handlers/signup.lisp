(util:def-file-package #:sykosomatic.handler.signup
  (:use :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.account
        :sykosomatic.template
        :sykosomatic.util.form)
  (:export :signup))

(defun render (form)
  (render-page "signup.html"
               (list :signup-fields
                     (list (text-field form :email)
                           (text-field form :display-name :max-length 32)
                           (text-field form :password :type "password"
                                       :value nil)
                           (text-field form :confirmation
                                       :value nil
                                       :type "password")))
               :title "Sign up for sykosomatic"))

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
              (render form)))))
    (:get
     (render (make-form 'signup)))))
