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
                     (list (text-input-field "email" "Email"
                                             :value (field-raw-value form :email)
                                             :error (field-error form :email))
                           (text-input-field "display-name" "Display Name"
                                             :max-length 32
                                             :value (field-raw-value form :display-name)
                                             :error (field-error form :display-name))
                           (text-input-field "password" "Password"
                                             :type "password"
                                             :error (field-error form :password))
                           (text-input-field "confirmation" "Confirm password"
                                             :type "password"
                                             :error (field-error form :confirmation))))
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
