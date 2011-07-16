(in-package :sykosomatic)
(defpackage :templ
  (:export :404 :/ :login :stage :role :scenes :view-scene :signup :newchar))

;;; General
(defun templ::page (title body-fun &optional head-fun)
  (with-yaclml-output-to-string
    (<:html :prologue "<!DOCTYPE html>"
     (<:head
      (<:title (<:ah title))
      (<:meta :http-equiv "Content-type"
              :content "text/html;charset=UTF-8")
      (<:link :rel "stylesheet" :type "text/css" :href "res/styles.css")
      (templ::jquery-libs)
      (when head-fun (funcall head-fun)))
     (<:body
      (funcall body-fun)))))

(defmacro defpage (name lambda-list (&rest head) title &body body)
  `(defun ,name ,lambda-list
     (templ::page ,title
                  (lambda ()
                    ,@body)
                  ,@(when head
                     `((lambda () ,@head))))))

(defun templ::jquery-libs ()
  (<:link :rel "stylesheet" :type "text/css" :href "res/css/smoothness/jquery-ui-1.8.14.custom.css")
  (<:script :type "text/javascript" :src "res/js/jquery-1.5.1.min.js")
  (<:script :type "text/javascript" :src "res/js/jquery-ui-1.8.14.custom.min.js"))

(defun templ::login-component ()
  (<:form :name "login" :action "/login" :method "post"
          (<:label (<:ah "Log in"))
          (<:br)
          (<:label (<:ah "Email"))
          (<:text :name "account-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:submit :value "Submit")))

(defun templ::logout-button ()
  (<:form :class "logout-button" :action "/logout" :method "post"
          (<:submit :class "btn" :value "Log Out")))

(defun templ::error-messages ()
  ;; This one's a bit leaky. It's pretty special, anyway.
  (when-let ((errors (session-value 'errors)))
    (<:ul :class "errors"
          (mapc (lambda (err) (<:li (<:ah err))) errors))
    (setf (session-value 'errors) nil)))

;;; 404
(defpage templ::404 () ()
    "404 Not Found"
  (<:p (<:ah "Sorry, that page does not exist.")))

;;; /
(defpage templ::/ () ()
    "Sykosomatic.org Dev Site"
  (<:p (<:ah "Sykosomatic is a cooperative storytelling system, currently in development. ")
       (<:href "/login" (<:ah "Log in.")))
  (<:p (<:ah "Already logged in? What are you doing here?! Head to the ")
       (<:href "/stage" (<:ah "Stage!"))))

;;; /login
(defpage templ::login () ()
    "Log in"
  (templ::error-messages)
  (templ::login-component)
  (<:href "/signup" (<:ah "Create account.")))

;;; /stage
(defpage templ::stage () ((templ::gameplay-js-libs))
    "All the World's a Stage"
  (templ::chat-area)
  #+nil
  (templ::scene-recording)
  #+nil
  (templ::scene-list-link)
  #+nil
  (templ::logout-button))

(defun templ::gameplay-js-libs ()
  ;; When you feel like figuring out why optional loading fails, take the following
  ;; two lines out...
  (<:script :type "text/javascript" :src "res/swfobject.js")
  (<:script :type "text/javascript" :src "res/web_socket.js")
  (<:script :type "text/javascript" :src "res/json2.js")
  (<:script :type "text/javascript" :src "res/client.js"))

(defun templ::chat-area ()
  (<:div :class "main-area"
         (<:div :class "chat-area"
                (<:div :class "chat-box" :id "chat-box")
                (templ::user-input-area))
         (templ::ooc-area)))

(defun templ::ooc-area ()
  (<:div :class "ooc-area"
         (<:div :class "ooc-display" :id "ooc-display")
         (<:form :class "user-input-area" :id "ooc-input-area"
                 :action "javascript:sykosomatic.send_ooc_input()"
                 (<:input :type "textarea" :id "ooc-input")
                 (<:submit :value "Send"))))

(defun templ::user-input-area ()
  ;; TODO - Maybe let client.js install send_input()?
  (<:form :id "user-input-area" :name "user-input-area" :action "javascript:sykosomatic.send_input()"
          (<:input :type "textarea" :id "user-input")
          (<:submit :value "Send")))

(defun templ::scene-recording ()
  ;; TODO - let client.js install these?
  (<:submit :class "btn" :id "start-recording" :value "Start Recording")
  (<:submit :class "btn" :id "stop-recording" :value "Stop Recording"))

(defun templ::scene-list-link ()
  (<:a :class "btn"
       :href "/scenes" "My Scenes"))

;;; /role
(defpage templ::role (characters) ()
    "What role shall you play?"
  (templ::error-messages)
  (templ::character-list characters)
  (<:href "/newchar" "Create a new character.")
  (templ::logout-button))

(defun templ::character-list (characters)
  (<:ul
   (mapc (lambda (char) (<:li (templ::character-link char)))
         characters)))

(defun templ::character-link (char)
  ;; TODO - This ~A smells really bad.
  (<:href (format nil "/stage?char=~A" (string-downcase char))
          (<:ah char)))

;;; /scenes
(defpage templ::scenes (scenes) ()
    "My scenes"
  (<:ul :class "scene-list"
        (mapc (lambda (scene)
                (<:li (<:href (format nil "/view-scene?id=~A" scene) (<:ah scene))))
              scenes)))

;;; /view-scene
(defpage templ::view-scene (scene display-vote-button-p scene-rating) ()
    "Viewing Scene"
  (templ::scene scene)
  (when display-vote-button-p
    (templ::scene-upvote scene))
  (when scene-rating
    (templ::scene-rating scene-rating)))

(defun templ::scene-rating (rating)
  (<:p (<:ah "Rating: " rating)))

(defun templ::scene-upvote (scene-id)
  (<:form :method "post" :action (format nil "/view-scene?id=~A" scene-id)
          (<:submit :value "Vote for Scene")))

(defun templ::user-action (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action)))
    (<:div :class "user-entry"
      (if (and (not (emptyp action)) (emptyp dialogue))
          (<:p :class "action"
               (<:ah (user-action-user user-action) " " action))
          (progn
            (<:p :class "character"
                 (<:ah (user-action-user user-action)))
            (unless (emptyp action)
              (<:p :class "parenthetical"
                   (<:ah "(" action ")")))
            (<:p :class "dialogue"
                 (<:ah
                  (if (emptyp dialogue)
                      "..."
                      dialogue))))))))

(defun templ::scene (id)
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
         (find-scene-entries id))))

;;; /signup
(defpage templ::signup () ()
    "Sign up"
  (templ::error-messages)
  (templ::signup-component))

(defun templ::signup-component ()
  (<:form :name "signup" :action "/signup" :method "post"
          (<:label (<:ah "Sign up:"))
          (<:br)
          (<:label (<:ah "Email"))
          (<:text :name "account-name")
          (<:br)
          (<:label (<:ah "Display name"))
          (<:text :name "display-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:label (<:ah "Confirm password"))
          (<:input :type "password" :name "confirmation")
          (<:br)
          (<:submit :value "Submit")))

;;; /newchar
(defpage templ::newchar () ()
    "Create a character"
  (templ::error-messages)
  (templ::character-creation-component))

(defun templ::character-creation-component ()
  (<:form :name "create-character" :action "/newchar" :method "post"
          (<:label (<:ah "Create a character."))
          (<:br)
          (<:label (<:ah "Name"))
          (<:text :name "name")
          (<:br)
          (<:label (<:ah "Description"))
          (<:input :type "textfield" :name "description")
          (<:br)
          (<:submit :value "Submit")))
