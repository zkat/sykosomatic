(cl:defpackage #:sykosomatic.handler.role
  (:use :cl :hunchentoot
        :sykosomatic.db
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.game-objects.nameable
        :sykosomatic.account)
  (:export :role))
(cl:in-package #:sykosomatic.handler.role)

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:render-template "role" (mapcar #'full-name
                                          (account-bodies (current-account))))))
