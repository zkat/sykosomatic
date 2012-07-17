(util:def-file-package #:sykosomatic.handler.login
  (:use :hunchentoot
        :sykosomatic.util.form
        :sykosomatic.db
        :sykosomatic.template
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.account)
  (:export :login))

(define-easy-handler (login :uri "/login") (email password)
  (case (print (request-method*))
    (:get
     (when-let ((account-id (current-account)))
       (push-error "Already logged in as ~A." (account-email account-id)))
     (let ((form (make-form 'signup)))
       (render-page "login.html"
                    (list :login-fields
                          (list (text-field form "Email")
                                (text-field form "Password"
                                            :type "password")))
                    :title "Login Page")))
    (:post
     (when (current-account)
       (redirect "/login"))
     (if-let ((account-id (validate-account email password)))
       (progn
         (start-persistent-session account-id)
         (logit "~A logged in." email)
         (redirect "/role"))
       (progn
         (push-error "Invalid login or password.")
         (redirect "/login"))))))
