(cl:defpackage #:sykosomatic.handler.stage
  (:use :cl :hunchentoot :alexandria
        :sykosomatic.account
        :sykosomatic.game-objects.nameable
        :sykosomatic.character
        :sykosomatic.websocket
        :sykosomatic.session
        :sykosomatic.handler)
  (:export :stage))
(cl:in-package #:sykosomatic.handler.stage)

(define-easy-handler (play :uri "/stage") ((char :parameter-type 'integer))
  (ensure-logged-in)
  (let ((character-id (when char (nth char (account-characters (current-account))))))
    (cond ((null character-id)
           (push-error "You must select a character before playing.")
           (redirect "/role"))
          (t (with-form-errors (templ:render-template "stage"
                                                      (generate-websocket-token (session-string))
                                                      (full-name character-id)))))))
