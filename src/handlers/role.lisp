(util:def-file-package #:sykosomatic.handler.role
  (:use :hunchentoot
        :sykosomatic.db
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.game-objects.nameable
        :sykosomatic.account)
  (:export :role))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:render-template "role" (mapcar #'full-name
                                          (account-bodies (current-account))))))
