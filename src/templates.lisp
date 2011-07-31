(cl:defpackage :sykosomatic.templ
  (:use :cl :yaclml :alexandria :string-case)
  (:nicknames :templ)
  (:export :not-found :home :login :stage :role
           :scenes :view-scene :signup :newchar
           :career-div :bodypart-div :bodypart-adj-select
           :newchar-preview-div))
(cl:in-package :templ)

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

;;; /newchar-preview
(defun possessive (pronoun)
  (string-case (pronoun)
    ("he" "his")
    ("she" "her")
    ("they" "their")))

(defun newchar-preview-div (&key first-name nickname last-name pronoun pluralp origin
                            parents siblings class
                            &aux (fully-named (notany #'emptyp
                                                      (list first-name nickname last-name pronoun))))
  (yaclml:with-yaclml-output-to-string
    (<:p (<:ah "You are creating a new character.")
         (<:ah (if fully-named
                   (format nil " ~:(~A~) name is ~:(~A~) ~:(~A~), also known as \"~A\"."
                           (possessive pronoun) first-name last-name nickname)
                   (format nil " ~:(~A~) ~A been fully named yet."
                           pronoun (if pluralp "haven't" "hasn't")))))
    (<:p (<:ah (format nil "~:(~A~) ~A originally from ~A."
                       pronoun (if pluralp "are" "is")
                       (cond
                         ((string= origin "local")
                          "the Twin Cities")
                         ((string= origin "state")
                          "Minnesota")
                         ((string= origin "midwest")
                          "somewhere in the Midwest")
                         ((string= origin "east-coast")
                          "the East Coast")
                         ((string= origin "south")
                          "the South")
                         ((string= origin "west-coast")
                          "the West Coast")
                         (t "outside the continental US")))))
    (<:p (<:ah (format nil "Brought up ~A ~A parent~P, ~:(~A~) was one of ~A
    children in a ~A family."
                       (string-case (parents)
                         ("none" "without")
                         (t "by"))
                       (string-case (parents)
                         ("more" "several")
                         ("none" "any")
                         (t parents))
                       (string-case (parents)
                         ("one" 1)
                         (t 3))
                       nickname
                       (string-case (siblings)
                         ("more" "several")
                         ("one" "two")
                         ("two" "three")
                         ("three" "four"))
                       class)))
    (<:p (<:ah (format nil "Now ~:(~A~) is ~A years old. ~:(~A~) ~A
      ~A. Also, ~(~A~) ~A ~A. Having built a career in ~A for the
      past ~A, ~A has ~A."
                       nickname "AGE" pronoun (if pluralp "have" "has") "FRIENDSHIP-STATUS"
                       pronoun (if pluralp "are" "is") "DATING-STATUS"
                       "CAREER1" "CAREER1-LENGTH" pronoun "OTHER-CAREERS")))
    (<:p (<:ah (format nil "~:(~A~) is currently staying in ~A."
                       nickname "HERE-AND-NOW")))
    (<:p (<:ah (format nil "~:(~A~)'s journey is about to begin." nickname)))))

;;; /newchar
(defun newchar-js ()
  (<:script :type "text/javascript" :src "res/newchar.js"))

(defpage newchar () ((newchar-js))
    "Create a character"
  (error-messages)
  #+nil(<:p :id "preview" "This is an experiment")
  (<:form :name "character-creation" :action "/newchar" :method "post"
   (<:div
    :id "creation-forms"
    #+nil(<:h3 "Identity")
    (cc-identity)
    #+nil(<:h3 "Early Life")
    (cc-early-life)
    #+nil(<:h3 "Later Life")
    (cc-later-life)
    #+nil(<:h3 "Appearance")
    (cc-appearance)
    #+nil(<:h3 "Here and Now")
    (cc-here-and-now)
    #+nil(<:h3 "Confirm")
    (cc-confirm))))

(defun cc-confirm ()
  (<:div
   :id "confirm"
   (<:fieldset
    (<:legend "Confirmation")
    (<:p "All done? Are you sure you wish to create this character?")
    (<:submit :class "button" :value "All Done"))))

(defun next-tab-button ()
  (<:button :type "button" :class "button next-tab" "Next"))

(defun cc-identity ()
  (<:div
   :id "identity"
   (<:fieldset
    (<:div :class "field"
           (<:label :for "pronoun" (<:ah "Pronoun"))
           (<:select :id "pronoun" :name "pronoun"
                     (<:option :value "" "Choose pronoun...")
                     (<:option :value "she" "She")
                     (<:option :value "he" "He")
                     (<:option :value "they" "They"))))
   (<:fieldset
    (<:legend "Name")
    (text-input-field "first-name" "First Name")
    (text-input-field "nickname" "Nickname" :max-length 24)
    (text-input-field "last-name" "Last Name"))))

(defun cc-early-life ()
   (<:div :id "early-life"
     (<:fieldset
      (<:legend (<:ah "Place of origin"))
      (<:div :class "field"
             (<:label :for "origin" (<:ah "Where from?"))
             (<:select :id "origin" :name "origin"
                       (<:option :value "" :selected "selected" (<:ah "Choose origin..."))
                       (<:option :value "local" (<:ah "Local -- is from the Twin Cities area."))
                       (<:option :value "state" (<:ah "Minnesotan -- not from the Cities, but still from the state."))
                       (<:option :value "midwest" (<:ah "Midwestern -- hails from elsewhere in the American Midwest."))
                       (<:option :value "east-coast" (<:ah "East Coast -- is from the east coast of the US."))
                       (<:option :value "south" (<:ah "Southern -- comes from the Southern US."))
                       (<:option :value "west-coast" (<:ah "West Coast -- California, Pacific Northwest, etc."))
                       (<:option :value "else" (<:ah "Elsewhere -- Alaska, Hawaii, or other countries.")))))
     (<:fieldset
      (<:legend (<:ah "Family situation"))
      (<:div :class "field"
             (<:label :for "parents" (<:ah "Number of parents"))
             (<:select :id "parents" :name "parents"
                       (<:option :value "" :selected "selected" (<:ah "Choose parents..."))
                       (<:option :value "none" "None")
                       (<:option :value "one" "One")
                       (<:option :value "two" "Two")
                       (<:option :value "more" "More than two")))
      (<:div :class "field"
             (<:label :for "siblings" (<:ah "Number of siblings"))
             (<:select :id "siblings" :name "siblings"
                       (<:option :value "" :selected "selected" (<:ah "Choose siblings..."))
                       (<:option :value "none" "None")
                       (<:option :value "one" "One")
                       (<:option :value "two" "Two")
                       (<:option :value "three" "Three")
                       (<:option :value "more" "More than three")))
      (<:div :class "field"
             (<:label :for "childhood-finances" (<:ah "Financial class"))
             (<:select :id "childhood-finances" :name "childhood-finances"
                       (<:option :value "" :selected "selected" (<:ah "Choose class..."))
                       (<:option :value "poor" "Poor")
                       (<:option :value "working-class" "Working Class")
                       (<:option :value "middle-class" "Middle Class")
                       (<:option :value "upper-class" "Upper Class"))))))

(defun career-div (idx &aux
                   (career-name (format nil "careers[~A]" idx))
                   (career-id (format nil "career-~A" idx))
                   (years-name (format nil "career-times[~A]" idx))
                   (years-id (format nil "career-times-~A" idx)))
  (<:div :class "field careers"
         (<:label :for career-id (<:ah "Career"))
         (<:select :name career-name :id career-id
                   (loop for (value . label) in '(("" . "Choose a career...")
                                                  ("lumberjack" . "Lumberjack")
                                                  ("programmer" . "Software Developer")
                                                  ("messiah" . "Savior"))
                      do (<:option :value value (<:ah label))))
         " for "
         (<:input :class "career-times" :name years-name :id years-id)
         " years."
         (<:button :type "button" (<:ah "remove"))))

(defun cc-later-life ()
  (<:div :id "later-life"
    (<:fieldset
     (<:legend "Friends and More")
     (<:div :class "field"
            (<:label :for "friends" (<:ah "Any friends?"))
            (<:select :id "friends" :name "friends"
                      (<:option :value "" :selected "selected" (<:ah "Choose friends..."))
                      (<:option :value "ronery" "No, character is all alone.")
                      (<:option :value "acquaintances" "Not really, just some acquaintances/coworkers and such.")
                      (<:option :value "tight" "Yeah, but just one, or a couple of very close friends.")
                      (<:option :value "social" "Yeah, the character has plenty of friends, but few are really close.")
                      (<:option :value "loved-by-everyone" "Yes. The character has a relatively big circle of acquaintances and close friends.")))
     (<:div :class "field"
            (<:label :for "so" (<:ah "Is there a special someone?"))
            (<:select :id "so" :name "so"
                      (<:option :value "" :selected "selected" (<:ah "Choose significant other..."))
                      (<:option :value "ronery" "No, the character is forever alone.")
                      (<:option :value "dating" "Kinda, currently seeing someone.")
                      (<:option :value "committed" "Yes. The character has been with someone for a while.")
                      (<:option :value "ball-and-chain"
                                "Yes, the character is in a committed relationship and/or married."))))
    (<:fieldset
     (<:legend :id "careers-desc" "Choose up to 5 careers")
     (<:button :type "button" :id "add-career" :class "button" "Add Career")
     (<:div :id "careers"
            (loop for i below 5
               do (career-div i))))))

(defun cc-appearance ()
  (<:div :id "appearance"
         (<:fieldset
          (<:legend :id "bodyparts-desc" (<:ah "Choose up to 5 distinguishing features"))
          (<:button :type "button" :id "add-bodypart" :class "button" "Add a feature")
          (<:div :id "bodyparts"
                 (loop for i below 5
                    do (bodypart-div i))))))

(defparameter *adjectives*
  (with-open-file (s (asdf:system-relative-pathname 'sykosomatic "features.txt"))
    (read s)))

(defun bodypart-div (idx &aux
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
                        *adjectives*))
         (<:select :name adj-name :id adj-id :class "bodypart-adjs"
                   (<:option :value "" (<:ah "Choose an adjective...")))
         (<:button :type "button" (<:ah "remove"))))

(defun bodypart-adj-select (feature)
  (<:option :value "" (<:ah "Choose an adjective..."))
  (loop for (category adjectives) in (cdr (assoc feature *adjectives* :test #'string-equal))
     do (<:optgroup :label (if (string= category "all") "general" category)
                    (map nil (lambda (adj)
                               (<:option :value adj (<:ah adj)))
                         adjectives))))

(defun cc-here-and-now ()
  (<:div :id "here-and-now"
         (<:fieldset
          (<:legend "Current location")
          (<:div :class "field"
                 (<:label :for "where" (<:ah "Where are they now?"))
                 (<:select :id "where" :name "where"
                           (<:option :value "" :selected "selected" (<:ah "Choose current location..."))
                           (<:option :value "midway" "Midway Area")
                           (<:option :value "downtown" "Downtown Minneapolis")
                           (<:option :value "dinkytown" "Dinkytown Neighborhood")
                           (<:option :value "riverfront" "Riverfront District")
                           (<:option :value "west-bank" "West Bank Neighborhood"))))))
