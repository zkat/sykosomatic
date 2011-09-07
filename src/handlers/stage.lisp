(cl:defpackage #:sykosomatic.handler.stage
  (:use :cl :hunchentoot :alexandria
        :sykosomatic.account
        :sykosomatic.character
        :sykosomatic.session
        :sykosomatic.handler)
  (:export :stage))
(cl:in-package #:sykosomatic.handler.stage)

(define-easy-handler (play :uri "/stage") (char)
  ;; TODO - make 'char' something other than the name. Some kind of external ID for the character.
  ;;        'find-character' is more useful for the parser, not as a unique identifier.
  (ensure-logged-in)
  (cond ((emptyp char)
         (push-error "You must select a character before playing.")
         (redirect "/role"))
        ((not (eql (character-account (find-character char))
                   (current-account)))
         (push-error "You're not authorized to play that character.")
         (redirect "/role"))
        (t (with-form-errors (templ:render-template "stage" char)))))
