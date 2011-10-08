(util:def-file-package #:sykosomatic.handler.role
  (:use :hunchentoot
        :sykosomatic.db
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.components.describable
        :sykosomatic.account
        :sykosomatic.template)
  (:export :role))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (render-page "role.html" (list :characters (loop for entity in (account-bodies (current-account))
                                                for i from 0
                                                collect (list :char-name (short-description
                                                                          entity entity)
                                                              :char-index i)))
               :title "Pick a role to play"))
