(cl:defpackage #:sykosomatic.handler.newchar
  (:use :cl :hunchentoot
        :alexandria
        :yaclml
        :sykosomatic.handler
        :sykosomatic.character-creation
        :sykosomatic.session
        :sykosomatic.account)
  (:export :newchar))
(cl:in-package #:sykosomatic.handler.newchar)

(define-easy-handler (newchar :uri "/newchar") (pronoun
                                                first-name
                                                nickname
                                                last-name
                                                origin
                                                parents
                                                siblings
                                                situation
                                                friends
                                                so
                                                (careers :parameter-type 'array)
                                                (career-times :parameter-type '(array integer))
                                                (features :parameter-type 'array)
                                                (feature-adjs :parameter-type 'array)
                                                where)
  (ensure-logged-in)
  (case (request-method*)
    (:get
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
    (:post
     (multiple-value-bind (char-created-p errors)
         (create-character (current-account)
                           :pronoun  pronoun
                           :first-name first-name
                           :nickname nickname
                           :last-name last-name
                           :origin origin
                           :parents parents
                           :siblings siblings
                           :situation situation
                           :friends friends
                           :so so
                           :career-info (map 'list #'cons careers career-times)
                           :feature-info (map 'list #'cons features feature-adjs)
                           :where where)
       (if char-created-p
           (progn
             (redirect "/role"))
           (progn
             (appendf (session-errors) errors)
             (redirect "/newchar")))))))

(define-easy-handler (newchar-feature-adjs :uri "/newchar/feature-adjs") (feature-name)
  (when-let (opts (cc-adjectives feature-name))
    (with-yaclml-output-to-string (templ:render-template "feature-adj-select" opts))))

(define-easy-handler (newchar-location-description :uri "/newchar/location-description") (loc)
  (when-let (desc (cc-location-description loc))
    (setf (content-type*) "text/plain")
    desc))
