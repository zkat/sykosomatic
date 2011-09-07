(cl:in-package :templ)

(sykosomatic.util:optimizations)

(defun newchar-js ()
  (<:script :type "text/javascript" :src "res/js/newchar.js"))

(defpage newchar (&key origins parents siblings situations
                       friends so careers
                       location-opts adjectives)
    ((newchar-js))
    "Create a character"
  (error-messages)
  #+nil(<:p :id "preview" "This is an experiment")
  (<:form :name "character-creation" :action "/newchar" :method "post"
   (<:div
    :id "creation-forms"
    (cc-identity '(("she" . "She")
                   ("he" . "He")
                   ("they" . "They")))
    (cc-early-life origins parents siblings situations)
    (cc-later-life friends so careers)
    (cc-appearance adjectives)
    (cc-here-and-now location-opts)
    (cc-confirm))))

(defun load-opts (opts)
  (map nil (lambda (opt)
             (<:option :value (option-value opt)
                       (<:ah (option-text opt))))
       opts))

(defun load-optgroups (optgroups)
  (map nil (lambda (optgroup)
             (<:optgroup :label (optgroup-label optgroup)
                         (load-opts (optgroup-options optgroup))))
       optgroups))

(defun cc-identity (genders)
  (<:div
   :id "identity"
   (<:fieldset
    (<:div :class "field"
           (<:label :for "pronoun" (<:ah "Pronoun"))
           (<:select :id "pronoun" :name "pronoun"
                     (<:option :value "they" "Choose pronoun...")
                     (load-opts genders))))
   (<:fieldset
    (<:legend "Name")
    (text-input-field "first-name" "First Name")
    (text-input-field "nickname" "Nickname" :max-length 24)
    (text-input-field "last-name" "Last Name"))))

(defun cc-early-life (origins parents siblings situations)
   (<:div :id "early-life"
     (<:fieldset
      (<:legend (<:ah "Place of origin"))
      (mk-select-field "origin" "Where from?" origins "Choose origin..."))
     (<:fieldset
      (<:legend (<:ah "Family situation"))
      (mk-select-field "parents" "Number of parents" parents "Choose parents...")
      (mk-select-field "siblings" "Number of siblings" siblings "Choose siblings...")
      (mk-select-field "situation" "Financial situation" situations "Choose situation..."))))

(defun career-div (idx careers &aux
                   (career-name (format nil "careers[~A]" idx))
                   (career-id (format nil "career-~A" idx))
                   (years-name (format nil "career-times[~A]" idx))
                   (years-id (format nil "career-times-~A" idx)))
  (<:div :class "field careers"
         (<:label :for career-id (<:ah "Career"))
         (mk-select career-name careers "Choose a career..." :id career-id)
         " for "
         (<:input :class "career-times" :name years-name :id years-id)
         " years."
         (<:button :type "button" (<:ah "remove"))))

(defun cc-later-life (friends so careers)
  (<:div :id "later-life"
    (<:fieldset
     (<:legend "Friends and More")
     (mk-select-field "friends" "Any friends?" friends "Choose friends...")
     (mk-select-field "so" "Special someone?" so "Choose significant other..."))
    (<:fieldset
     (<:legend :id "careers-desc" "Choose up to 5 careers")
     (<:button :type "button" :id "add-career" :class "button" "Add Career")
     (<:div :id "careers"
            (loop for i below 5
               do (career-div i careers))))))

(defun bodypart-div (idx adjectives &aux
                     (bodypart-name (format nil "bodyparts[~A]" idx))
                     (bodypart-id (format nil "bodyparts-~A" idx))
                     (adj-name (format nil "bodypart-adjs[~a]" idx))
                     (adj-id (format nil "bodypart-adjs-~a" idx)))
  (<:div :class "field bodyparts"
         (<:label :for bodypart-id (<:ah (format nil "Feature")))
         (<:select :name bodypart-name :id bodypart-id :class "bodypart-name"
                   (<:option :value "" (<:ah "Choose feature..."))
                   (map nil (lambda (entry &aux (val (car entry)))
                              (<:option :value val (<:ah val)))
                        adjectives))
         (mk-select adj-name nil "Choose an adjective..." :id adj-id :class "bodypart-adjs")
         (<:button :type "button" (<:ah "remove"))))

(defun cc-appearance (adjectives)
  (<:div :id "appearance"
         (<:fieldset
          (<:legend :id "bodyparts-desc" (<:ah "Choose up to 5 distinguishing features"))
          (<:button :type "button" :id "add-bodypart" :class "button" "Add a feature")
          (<:div :id "bodyparts"
                 (loop for i below 5
                    do (bodypart-div i adjectives))))))

(defun bodypart-adj-select (adjective-categories)
  (<:option :value "" (<:ah "Choose an adjective..."))
  (loop for (category adjectives) in adjective-categories
     do (<:optgroup :label (if (string= category "all") "general" category)
                    (map nil (lambda (adj)
                               (<:option :value adj (<:ah adj)))
                         adjectives))))

(defun cc-here-and-now (locations)
  (<:div :id "here-and-now"
         (<:fieldset
          (<:legend "Current location")
          (mk-select-field "where" "Where are they now?"
                           locations "Choose current location...")
          (<:h3 :id "description-header" "Description")
          (<:p :id "location-description" #+aria(:aria-live "polite" :aria-relevant "additions removals"
                                                      :aria-describedby "description-header")))))

(defun cc-confirm ()
  (<:div
   :id "confirm"
   (<:fieldset
    (<:legend "Confirmation")
    (<:p "All done? Are you sure you wish to create this character?")
    (<:submit :class "button" :value "All Done"))))
