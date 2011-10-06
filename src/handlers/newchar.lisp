(util:def-file-package #:sykosomatic.handler.newchar
  (:use :hunchentoot
        :yaclml
        :sykosomatic.handler
        :sykosomatic.character-creation
        :sykosomatic.session
        :sykosomatic.account
        :sykosomatic.util.form)
  (:export :newchar))

(defun newchar-select-options ()
  (list :pronouns (cc-select-options "pronoun")
        :origins (cc-select-options "origin")
        :parents (cc-select-options "parents")
        :siblings (cc-select-options "siblings")
        :situations (cc-select-options "situation")
        :careers (cc-select-options "career")
        :friends (cc-select-options "friends")
        :so (cc-select-options "significant-other")
        :location-opts (cc-select-options "location")
        :features (cc-features)))

(define-easy-handler (newchar :uri "/newchar") ()
  (ensure-logged-in)
  (case (request-method*)
    (:post
     (let ((form (make-form 'newchar (post-parameters*))))
       (cond ((form-valid-p form)
              (create-character (current-account) form)
              (redirect "/role"))
             (t
              (templ:render-template "newchar" form (newchar-select-options))))))
    (:get
     (with-form-errors
       (templ:render-template "newchar" (make-form 'newchar) (newchar-select-options))))))

(define-easy-handler (newchar-feature-adjs :uri "/newchar/feature-adjs") (feature-name)
  (when-let (opts (cc-adjectives feature-name))
    (with-yaclml-output-to-string (templ:render-template "feature-adj-select" opts))))

(define-easy-handler (newchar-location-description :uri "/newchar/location-description") (loc)
  (when-let (desc (cc-location-description loc))
    (setf (content-type*) "text/plain")
    desc))
