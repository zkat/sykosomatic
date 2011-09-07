(cl:defpackage #:sykosomatic.handler.role
  (:use :cl :hunchentoot
        :sykosomatic.db
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.character
        :sykosomatic.account)
  (:export :role))
(cl:in-package #:sykosomatic.handler.role)

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-db ()
    (with-form-errors
      (templ:render-template "role" (mapcar #'character-name
                                            (account-characters (current-account)))))))
