(cl:defpackage #:sykosomatic.handler.newchar
  (:use :cl :hunchentoot
        :alexandria
        :yaclml
        :sykosomatic.handler
        :sykosomatic.character
        :sykosomatic.session
        :sykosomatic.account)
  (:export :newchar))
(cl:in-package #:sykosomatic.handler.newchar)

(define-easy-handler (newchar :uri "/newchar") ()
  #+nil((careers :parameter-type 'array)
        (career-times :parameter-type 'array)
        (bodyparts :parameter-type 'array)
        (bodypart-adjs :parameter-type 'array))
  #+nil(ensure-logged-in)
  (with-form-errors
    (templ:render-template "newchar"
                           :pronouns (cc-select-options "pronoun")
                           :origins (cc-select-options "origin")
                           :parents (cc-select-options "parents")
                           :siblings (cc-select-options "siblings")
                           :situations (cc-select-options "situation")
                           :careers (cc-select-options "career")
                           :friends (cc-select-options "friends")
                           :so (cc-select-options "significant-other")
                           :location-opts (cc-select-options "location")
                           :features (cc-features))))

(define-easy-handler (newchar-feature-adjs :uri "/newchar/feature-adjs") (feature-name)
  (when-let (opts (cc-adjectives feature-name))
    (with-yaclml-output-to-string (templ:render-template "feature-adj-select" opts))))

(define-easy-handler (newchar-location-description :uri "/newchar/location-description") (loc)
  (when-let (desc (cc-location-description loc))
    (setf (content-type*) "text/plain")
    desc))
