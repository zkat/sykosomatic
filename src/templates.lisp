(cl:defpackage :sykosomatic.templ
  (:use :cl :yaclml :alexandria :string-case)
  (:nicknames :templ)
  (:export :not-found :home :login :stage :role
           :scenes :view-scene :signup :newchar
           :career-div :bodypart-div :bodypart-adj-select
           :newchar-preview-div

           :optgroup-label
           :optgroup-options
           :option-value
           :option-text))
(cl:in-package :templ)

(defgeneric optgroup-label (optgroup))
(defgeneric optgroup-options (optgroup))
(defgeneric option-value (opt)
  (:documentation "The text to be used for the :value attribute of this opt.")
  (:method ((opt cons))
    (car opt)))
(defgeneric option-text (opt)
  (:documentation "Used for the label of the opt.")
  (:method ((opt cons))
    (cdr opt)))

;;; General
(defun page (title body-fun &optional head-fun)
  (with-yaclml-output-to-string
    (<:html :prologue "<!DOCTYPE html>"
     (<:head
      (<:title (<:ah title))
      (<:meta :http-equiv "Content-type"
              :content "text/html;charset=UTF-8")
      (<:link :rel "stylesheet" :type "text/css" :href "res/styles.css")
      (jquery-libs)
      (when head-fun (funcall head-fun)))
     (<:body
      (funcall body-fun)))))

(defmacro defpage (name lambda-list (&rest head) title &body body)
  `(defun ,name ,lambda-list
     (page ,title
                  (lambda ()
                    ,@body)
                  ,@(when head
                     `((lambda () ,@head))))))

(defun jquery-libs ()
  (<:link :rel "stylesheet" :type "text/css" :href "res/css/smoothness/jquery-ui-1.8.14.custom.css")
  (<:link :rel "stylesheet" :type "text/css" :href "res/chosen.css")
  (<:script :type "text/javascript" :src "res/js/jquery-1.5.1.min.js")
  (<:script :type "text/javascript" :src "res/js/chosen.jquery.min.js")
  (<:script :type "text/javascript" :src "res/js/jquery-ui-1.8.14.custom.min.js"))

(defun logout-button ()
  (<:form :class "logout-button" :action "/logout" :method "post"
          (<:submit :class "btn" :value "Log Out")))

(defun error-messages ()
  ;; This one's a bit leaky. It's pretty special, anyway.
  (when-let ((errors (sykosomatic::session-value 'sykosomatic::errors)))
    (<:ul :class "errors"
          (mapc (lambda (err) (<:li (<:ah err))) errors))
    (setf (sykosomatic::session-value 'sykosomatic::errors) nil)))

(defun text-input-field (name label &key (type "text") max-length)
  (<:div :class "field"
    (<:label :for name (<:ah label))
    (<:input :name name :id name :type type :maxlength max-length)))

(defun mk-select (name opts default-opt-label &key (id name) class)
  (<:select :id id :name name :class class
            (<:option :value "" :selected "selected" (<:ah default-opt-label))
            (load-opts opts)))

(defun mk-select-field (name label opts default-opt-label &key (id name))
  (<:div :class "field"
         (<:label :for id (<:ah label))
         (mk-select name opts default-opt-label :id id)))

;;; 404
(defpage not-found () ()
    "404 Not Found"
  (<:p (<:ah "Sorry, that page does not exist.")))

;;; /
(defpage home () ()
    "Sykosomatic.org Dev Site"
  (<:p (<:ah "Sykosomatic is a cooperative storytelling system, currently in development. ")
        (<:href "/login" (<:ah "Log in.")))
  (<:p (<:ah "Already logged in? What are you doing here?! Go pick a ")
       (<:href "/role" (<:ah "character!"))))

;;; /login
(defpage login () ()
    "Log in"
  (error-messages)
  (login-component)
  (<:href "/signup" (<:ah "Create account.")))

(defun login-component ()
  (<:form :name "login" :action "/login" :method "post"
          (text-input-field "account-name" "Email")
          (text-input-field "password" "Password" :type "password")
          (<:submit :value "Log in")))

;;; /stage
(defpage stage (char-name) ((gameplay-js-libs))
    "All the World's a Stage"
  (chat-area char-name)
  #+nil
  (scene-recording)
  #+nil
  (scene-list-link)
  #+nil
  (logout-button))

(defun gameplay-js-libs ()
  ;; When you feel like figuring out why optional loading fails, take the following
  ;; two lines out...
  (<:script :type "text/javascript" :src "res/swfobject.js")
  (<:script :type "text/javascript" :src "res/web_socket.js")
  (<:script :type "text/javascript" :src "res/json2.js")
  (<:script :type "text/javascript" :src "res/client.js"))

(defun chat-area (char-name)
  (<:div :id "main-game-panel"
         (game-panel char-name)
         (ooc-panel)))

(defun game-panel (char-name)
  (<:div :id "game-panel"
         (<:div :class "scene-display" :id "scene-display")
         (<:div :id "game-input"
                (<:ul
                 (<:li (<:href "#dialogue-tab" "Dialogue"))
                 (<:li (<:href "#action-tab" "Action"))
                 (<:li (<:href "#ooc-tab" "OOC"))
                 (<:li (<:href "#parser-tab" "Parser")))
                (<:div :id "dialogue-tab"
                       (<:form :id "dialogue-input"
                               (<:p :class "character" (<:ah (string-capitalize char-name)))
                               (<:input)))
                (<:div :id "action-tab"
                       (<:form :id "action-input"
                               (<:ah (string-capitalize char-name))
                               (<:input)))
                (<:div :id "ooc-tab"
                       (<:form :id "ooc-input"
                               "OOC - " (<:input)))
                (<:div :id "parser-tab"
                       (<:form :id "parser-input"
                               (<:input))))))

(defun ooc-panel ()
  (<:div :id "ooc-panel"
         (<:div :id "ooc-display")))

(defun scene-recording ()
  (<:submit :class "btn" :id "start-recording" :value "Start Recording")
  (<:submit :class "btn" :id "stop-recording" :value "Stop Recording"))

(defun scene-list-link ()
  (<:a :class "btn"
       :href "/scenes" "My Scenes"))

;;; /role
(defpage role (characters) ()
    "What role shall you play?"
  (error-messages)
  (<:h2 (<:ah "Choose a character"))
  (character-list characters)
  (<:href "/newchar" "Create a new character.")
  (logout-button))

(defun character-list (characters)
  (<:ul
   (mapc (lambda (char) (<:li (character-link char)))
         characters)))

(defun character-link (char)
  ;; TODO - This ~A smells really bad.
  (<:href (format nil "/stage?char=~A" (string-downcase char))
          (<:ah char)))

;;; /scenes
(defpage scenes (scenes) ()
    "My scenes"
  (<:ul :class "scene-list"
        (mapc (lambda (scene)
                (<:li (<:href (format nil "/view-scene?id=~A" scene) (<:ah scene))))
              scenes)))

;;; /view-scene
(defpage view-scene (scene display-vote-button-p scene-rating) ()
    "Viewing Scene"
  (scene scene)
  (when display-vote-button-p
    (scene-upvote scene))
  (when scene-rating
    (scene-rating scene-rating)))

(defun scene-rating (rating)
  (<:p (<:ah "Rating: " rating)))

(defun scene-upvote (scene-id)
  (<:form :method "post" :action (format nil "/view-scene?id=~A" scene-id)
          (<:submit :value "Vote for Scene")))

;; TODO - fixme
(defun user-action (user-action)
  (let ((action (sykosomatic::user-action-action user-action))
        (dialogue (sykosomatic::user-action-dialogue user-action)))
    (<:div :class "user-entry"
      (if (and (not (emptyp action)) (emptyp dialogue))
          (<:p :class "action"
               (<:ah (sykosomatic::user-action-user user-action) " " action))
          (progn
            (<:p :class "character"
                 (<:ah (sykosomatic::user-action-user user-action)))
            (unless (emptyp action)
              (<:p :class "parenthetical"
                   (<:ah "(" action ")")))
            (<:p :class "dialogue"
                 (<:ah
                  (if (emptyp dialogue)
                      "..."
                      dialogue))))))))

;; TODO - fixme
(defun scene (id)
  ;; TODO - THIS IS DANGEROUS AND LEAKY. XSS GALORE.
  (<:script :type "text/javascript" :src "res/client.js")
  (<:div :class "chat-box" :id "chat-box")
  (<:script
   (mapc (lambda (entry-obj)
           (let ((entry-type (jsown:val entry-obj "entry_type")))
             (cond ((equal entry-type "dialogue")
                    (let ((actor (jsown:val entry-obj "actor"))
                          (dialogue (jsown:val entry-obj "dialogue"))
                          (paren (jsown:val entry-obj "parenthetical")))
                      ;; TODO - This smells a bit.
                      (<:ah (format nil "sykosomatic.add_dialogue(~S,~S~@[,~S~]);~%"
                                    actor dialogue paren))))
                   ((equal entry-type "action")
                    (let ((actor (jsown:val entry-obj "actor"))
                          (action-txt (jsown:val entry-obj "action")))
                      ;; TODO - Smelly, smelly.
                      (<:ah (format nil "sykosomatic.add_action(~S,~S);~%" actor action-txt)))))))
         (sykosomatic::find-scene-entries id))))

;;; /signup
(defpage signup () ()
    "Sign up"
  (error-messages)
  (signup-component))

(defun signup-component ()
  (<:form :name "signup" :action "/signup" :method "post"
          (<:fieldset
           (<:legend "Sign up!")
           (text-input-field "account-name" "Email")
           (text-input-field "display-name" "Display Name" :max-length 32)
           (text-input-field "password" "Password" :type "password")
           (text-input-field "confirmation" "Confirm password" :type "password"))
          (<:submit :value "Submit")))

;;; /newchar
(defun newchar-js ()
  (<:script :type "text/javascript" :src "res/newchar.js"))

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
          (<:p :id "location-description" :aria-live "polite" :aria-relevant "additions removals"
               :aria-describedby "description-header"))))

(defun cc-confirm ()
  (<:div
   :id "confirm"
   (<:fieldset
    (<:legend "Confirmation")
    (<:p "All done? Are you sure you wish to create this character?")
    (<:submit :class "button" :value "All Done"))))
