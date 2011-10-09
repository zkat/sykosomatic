(util:def-file-package #:sykosomatic.handler.newchar
  (:use :hunchentoot
        :yaclml
        :sykosomatic.handler
        :sykosomatic.character-creation
        :sykosomatic.session
        :sykosomatic.account
        :sykosomatic.util.form
        :sykosomatic.template)
  (:export :newchar))

(defun render (form)
  (render-page
   "newchar.html"
   `(:additional-js ((:js-path "res/js/newchar.js"))
     :pronoun-field (,(select-field "pronoun" "Pronoun"
                                    :default-text "Choose pronoun..."
                                    :id "pronoun"
                                    :optgroups
                                    (list (field-optgroup "Pronouns"
                                                          (cc-select-options "pronoun")))))
     :name-field (,(text-input-field "name" "Full Name"
                                     :value (field-raw-value form :name)
                                     :error (field-error form :name)))
     :origin-field (,(select-field "origin" "Where From?"
                                   :default-text "Choose origin..."
                                   :optgroups (list (field-optgroup
                                                     nil
                                                     (cc-select-options "origin")))))
     :parents-field (,(select-field "parents" "Number of parents"
                                    :default-text "Choose parents..."
                                    :optgroups (list (field-optgroup
                                                      nil
                                                      (cc-select-options "parents")))))
     :siblings-field (,(select-field "siblings" "Number of siblings"
                                     :default-text "Choose siblings..."
                                     :optgroups (list (field-optgroup
                                                       nil
                                                       (cc-select-options "siblings")))))
     :situation-field (,(select-field "situation" "Financial situation"
                                      :default-text "Choose situation..."
                                      :optgroups (list (field-optgroup
                                                        nil
                                                        (cc-select-options "situation")))))
     :friends-field (,(select-field "friends" "Any friends?"
                                    :default-text "Choose friends..."
                                    :optgroups (list (field-optgroup
                                                      nil
                                                      (cc-select-options "friends")))))
     :so-field (,(select-field "so" "Special someone?"
                               :default-text "Choose so..."
                               :optgroups (list (field-optgroup
                                                 nil
                                                 (cc-select-options "significant-other")))))
     :career-divs ,(loop for i below 5 collect
                        (list :career-field
                              (list (select-field (format nil "careers[~A]" i)
                                                  "Career"
                                                  :id (format nil "careers-~A" i)
                                                  :optgroups (list (field-optgroup
                                                                    nil
                                                                    (cc-select-options "career")))))
                              :career-time-field-name (format nil "career-times[~A]" i)
                              :career-time-field-id (format nil "career-times-~A" i)))
     :feature-divs ,(loop for i below 5 collect
                         (list :feature-field
                               (list (select-field (format nil "features[~A]" i)
                                                   "Feature"
                                                   :id (format nil "features-~A" i)
                                                   :optgroups (list (field-optgroup
                                                                     nil
                                                                     (loop for feature in (cc-features)
                                                                        collect (list :option-value feature
                                                                                      :option-text feature))))))
                               :adj-field-name (format nil "feature-adjs[~A]" i)
                               :adj-field-label "Choose an adjective..."
                               :adj-field-id (format nil "feature-adjs-~A" i)))
     :where-field (,(select-field "where" "Where are they now?"
                                  :default-text "Choose current location..."
                                  :optgroups (list (field-optgroup
                                                    nil
                                                    (cc-select-options "location"))))))
   :title "Character Creation"))

(define-easy-handler (newchar :uri "/newchar") ()
  (ensure-logged-in)
  (case (request-method*)
    (:post
     (let ((form (make-form 'newchar (post-parameters*))))
       (cond ((form-valid-p form)
              (create-character (current-account) form)
              (redirect "/role"))
             (t
              (render form)))))
    (:get
     (render (make-form 'newchar)))))

(define-easy-handler (newchar-feature-adjs :uri "/newchar/feature-adjs") (feature-name)
  (when-let (opts (cc-adjectives feature-name))
    ;; TODO - do this with templates?... Meh.
    (with-yaclml-output-to-string
      (<:option :value "" (<:ah "Choose an adjective..."))
      (loop for (category adjectives) in opts
         do (<:optgroup :label (if (string= category "all") "general" category)
                        (map nil (lambda (adj)
                                   (<:option :value adj (<:ah adj)))
                             adjectives))))))

(define-easy-handler (newchar-location-description :uri "/newchar/location-description") (loc)
  (when-let (desc (cc-location-description loc))
    (setf (content-type*) "text/plain")
    desc))
