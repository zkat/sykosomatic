(cl:defpackage :sykosomatic.templ
  (:use :cl :yaclml :alexandria)
  (:nicknames :templ)
  (:export :not-found :home :login :stage :role :scenes :view-scene :signup :newchar))
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
(defpage stage () ((gameplay-js-libs))
    "All the World's a Stage"
  (chat-area)
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

(defun chat-area ()
  (<:div :id "main-game-panel"
         (game-panel)
         (ooc-panel)))

(defun game-panel ()
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
                               (<:p :class "character" "<YOUR NAME>")
                               (<:input)))
                (<:div :id "action-tab"
                       (<:form :id "action-input"
                               "<your name> "
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
  (<:h2 (<:ah "Sign up"))
  (<:form :name "signup" :action "/signup" :method "post"
          (text-input-field "account-name" "Email")
          (text-input-field "display-name" "Display Name" :max-length 32)
          (text-input-field "password" "Password" :type "password")
          (text-input-field "confirmation" "Confirm password" :type "password")
          (<:submit :value "Submit")))

;;; /newchar
(defun creation-breadcrumb (focused)
  (<:ul :class "breadcrumbs"
   (loop
      for i from 0
      for title in '("Name" "Early life" "Later life" "Appearance")
      do (<:li :class (if (= i focused)
                          "breadcrumb-focused"
                          "breadcrumb-unfocused")
               (<:ah title)))))

(defpage newchar () ()
    "Create a character"
  (error-messages)
  (creation-breadcrumb 0)
  (character-creation-component))

(defun character-creation-component ()
  (<:form :name "create-character" :action "/newchar" :method "post"
          ;; Pronoun
          (<:div :class "field"
           (<:label :for "pronoun" (<:ah "Pronoun"))
           (<:select :id "pronoun" :name "pronoun"
                     (<:option :value "she" "She")
                     (<:option :value "he" "He")
                     (<:option :value "they" :selected "selected" "They")))
          ;; Name
          (<:div
           (<:br)
           (text-input-field "first-name" "First Name")
           (text-input-field "nickname" "Nickname" :max-length 24)
           (text-input-field "last-name" "Last Name"))
          ;; Early life
          ;; TODO
          (<:div
           (<:br)
           (text-input-field "origin" "Where from?")
           (text-input-field "parents" "Parents?")
           (text-input-field "siblings" "Siblings?")
           (text-input-field "economics" "Financial situation?"))

          ;; Later life
          ;; TODO
          (<:div
           (<:br)
           (text-input-field "tc-arrival" "How end up in Twin Cities?")
           (text-input-field "where-now" "Where in the Twin Cities?")
           (text-input-field "friends" "Any friends?")
           (text-input-field "so" "Significant other?")
           (<:br)
           (text-input-field "career1" "What career?")
           (text-input-field "career1-time" "How long?")
           (<:br)
           (text-input-field "career2" "What other career?")
           (text-input-field "career2-time" "How long?")
           (<:br)
           (text-input-field "career3" "What other career?")
           (text-input-field "career3-time" "How long?"))

          ;; Appearance
          ;; TODO
          (<:div
           (<:br)
           (text-input-field "bodypart1" "Notable feature")
           (text-input-field "bodypart1-adj" "Adjective")
           (<:br)
           (text-input-field "bodypart2" "Notable feature")
           (text-input-field "bodypart2-adj" "Adjective")
           (<:br)
           (text-input-field "bodypart3" "Notable feature")
           (text-input-field "bodypart3-adj" "Adjective"))

          (<:submit :class "submit" :value "Done")))
