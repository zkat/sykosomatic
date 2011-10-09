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

;;;
;;; New newchar stuff
;;;

;;; Pronoun
(defun render-pronoun (form)
  (render-page "newchar/pronoun.html"
               `(:action-page "/newchar/pronoun"
                 :pronoun-select
                 (,(select-field "pronoun" "Pronoun"
                                 :default-text "Choose pronoun..."
                                 :id "pronoun"
                                 :error (field-error form :pronoun)
                                 :value (field-raw-value form :pronoun)
                                 :optgroups
                                 (list (field-optgroup "Pronouns"
                                                       (cc-select-options "pronoun"))))))))
(define-easy-handler (newchar.pronoun :uri "/newchar/pronoun") ()
  (case (request-method*)
    (:get (render-pronoun (make-form 'pronoun)))
    (:post (let ((form (make-form 'pronoun (post-parameters*))))
             (cond ((form-valid-p form)
                    (print "Valid pronoun page")
                    (redirect "/newchar/growing-up"))
                   (t (render-pronoun form)))))))

;;; Growing Up
(defun render-growing-up ()
  (render-page
   "newchar/growing-up.html"
   `(:action-page "/newchar/growing-up"
     :origin-select (,(select-field "origin" "Where From?"
                                    :default-text "Choose origin..."
                                    :optgroups (list (field-optgroup
                                                      nil
                                                      (cc-select-options "origin")))))
     :parents-select (,(select-field "parents" "Number of parents"
                                     :default-text "Choose parents..."
                                     :optgroups (list (field-optgroup
                                                       nil
                                                       (cc-select-options "parents")))))
     :siblings-select (,(select-field "siblings" "Number of siblings"
                                      :default-text "Choose siblings..."
                                      :optgroups (list (field-optgroup
                                                        nil
                                                        (cc-select-options "siblings")))))
     :finances-select (,(select-field "finances" "Financial situation"
                                       :default-text "Choose financial situation..."
                                       :optgroups (list (field-optgroup
                                                         nil
                                                         (cc-select-options "situation"))))))))

(define-easy-handler (newchar.growing-up :uri "/newchar/growing-up") ()
  (case (request-method*)
    (:get (render-growing-up))
    (:post (redirect "/newchar/career"))))

;;; Career
(defun render-career ()
  (render-page
   "newchar/career.html"
   `(:action-page "/newchar/career"
     :additional-js ((:js-path "/res/js/newchar.js"))
     :career-divs ,(loop for i below 5 collect
                        (list :career-field
                              (list (select-field (format nil "careers[~A]" i)
                                                  "Career"
                                                  :id (format nil "careers-~A" i)
                                                  :optgroups (list (field-optgroup
                                                                    nil
                                                                    (cc-select-options "career")))))
                              :career-time-field-name (format nil "career-times[~A]" i)
                              :career-time-field-id (format nil "career-times-~A" i))))))

(define-easy-handler (newchar.career :uri "/newchar/career") ()
  (case (request-method*)
    (:get (render-career))
    (:post (redirect "/newchar/relationships"))))

;;; Relationships
(defun render-relationships ()
  (render-page
   "newchar/relationships.html"
   `(:action-page "/newchar/relationships"
     :additional-js ((:js-path "/res/js/newchar.js"))
     :friends-select (,(select-field "friends" "Any friends?"
                                    :default-text "Choose friends..."
                                    :optgroups (list (field-optgroup
                                                      nil
                                                      (cc-select-options "friends")))))
     :romance-select (,(select-field "romance" "Special someone?"
                                     :default-text "Choose relationship..."
                                     :optgroups (list (field-optgroup
                                                       nil
                                                       (cc-select-options "significant-other"))))))))

(define-easy-handler (newchar.relationships :uri "/newchar/relationships") ()
  (case (request-method*)
    (:get (render-relationships))
    (:post (redirect "/newchar/features"))))

;;; Features
(defun render-features ()
  (render-page
   "newchar/features.html"
   `(:action-page "/newchar/features"
     :additional-js ((:js-path "/res/js/newchar.js"))
     :feature-divs ,(loop for i below 5 collect
                         (list :feature-field
                               (list (select-field (format nil "features[~A]" i)
                                                   "Feature"
                                                   :id (format nil "features-~A" i)
                                                   :class "feature-name"
                                                   :optgroups (list (field-optgroup
                                                                     nil
                                                                     (loop for feature in (cc-features)
                                                                        collect (list :option-value feature
                                                                                      :option-text feature))))))
                               :adj-field-name (format nil "feature-adjs[~A]" i)
                               :adj-field-label "Choose an adjective..."
                               :adj-field-id (format nil "feature-adjs-~A" i))))))

(define-easy-handler (newchar.features :uri "/newchar/features") ()
  (case (request-method*)
    (:get (render-features))
    (:post (redirect "/newchar/location"))))

;;; Location
(defun render-location ()
  (render-page
   "newchar/location.html"
   `(:action-page "/newchar/location"
     :additional-js ((:js-path "/res/js/newchar.js"))
     :where-field (,(select-field "where" "Where are they now?"
                                  :default-text "Choose current location..."
                                  :optgroups (list (field-optgroup
                                                    nil
                                                    (cc-select-options "location"))))))))

(define-easy-handler (newchar.location :uri "/newchar/location") ()
  (case (request-method*)
    (:get (render-location))
    (:post (redirect "/newchar/name-and-confirm"))))

;;; Name and Confirm
(defun render-name-and-confirm ()
  (render-page
   "newchar/name-and-confirm.html"
   `(:action-page "/newchar/name-and-confirm"
     :additional-js ((:js-path "/res/js/newchar.js"))
     :char-preview "«An exciting new character»"
     :full-name-field (,(text-input-field "full-name" "Full Name"))
     :nickname-field (,(text-input-field "nickname" "Nickname")))))

(define-easy-handler (newchar.name-and-confirm :uri "/newchar/name-and-confirm") ()
  (case (request-method*)
    (:get (render-name-and-confirm))
    (:post (redirect "/newchar/pronoun"))))

;;;
;;; Old newchar
;;;
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
                                                   :class "feature-name"
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
