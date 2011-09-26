(cl:defpackage #:sykosomatic.handler.stage
  (:use :cl :hunchentoot :alexandria
        :sykosomatic.account
        :sykosomatic.character
        :sykosomatic.session
        :sykosomatic.handler)
  (:export :stage))
(cl:in-package #:sykosomatic.handler.stage)

(define-easy-handler (play :uri "/stage") ((char :parameter-type 'integer))
  ;; TODO - make 'char' something other than the name. Some kind of external ID for the character.
  ;;        'find-character' is more useful for the parser, not as a unique identifier.
  (ensure-logged-in)
  (let ((character-id (when char (nth char (account-characters (current-account))))))
    (cond ((null character-id)
           (push-error "You must select a character before playing.")
           (redirect "/role"))
          (t (with-form-errors (templ:render-template "stage" (character-name character-id)))))))
