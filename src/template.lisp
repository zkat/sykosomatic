(util:def-file-package :sykosomatic.template
  (:use :yaclml :string-case
        :sykosomatic.util.form)
  (:nicknames :templ)
  (:export :not-found :index :login :stage :role
           :scenes :view-scene :signup :newchar
           :career-div :bodypart-div :bodypart-adj-select
           :newchar-preview-div

           :render-template

           :*errors*

           :optgroup-label
           :optgroup-options
           :option-value
           :option-text
           :option-detail))

(defgeneric optgroup-label (optgroup))
(defgeneric optgroup-options (optgroup))
(defgeneric option-value (opt)
  (:documentation "The text to be used for the :value attribute of this opt.")
  (:method ((opt cons))
    (car opt)))
(defgeneric option-text (opt)
  (:documentation "Used for the label of the opt.")
  (:method ((opt cons))
    (second opt)))
(defgeneric option-detail (opt)
  (:documentation "Used for more verbose display of what the option is.")
  (:method ((opt cons))
    (third opt)))

(defvar *templates* (make-hash-table :test #'equalp))

(defun add-template (name function)
  (setf (gethash name *templates*) function))
(defun remove-template (name)
  (remhash name *templates*))
(defun find-template (name)
  (gethash name *templates*))

(defun render-template (name &rest template-args)
  (when-let (template-fun (find-template name))
    (apply template-fun template-args)))

;;; General
(defun page (title body-fun &optional head-fun)
  (with-yaclml-output-to-string
    (<:html :prologue "<!DOCTYPE html>"
     (<:head
      (<:title (<:ah title))
      (<:meta :http-equiv "Content-type"
              :content "text/html;charset=UTF-8")
      (<:link :rel "stylesheet" :type "text/css" :href "res/css/styles.css")
      (jquery-libs)
      (when head-fun (funcall head-fun)))
     (<:body
      (funcall body-fun)))))

(defmacro defpage (name lambda-list (&rest head) title &body body)
  `(add-template ,(string name)
                 (lambda ,lambda-list
                   (page ,title
                         (lambda ()
                           ,@body)
                         ,@(when head
                                 `((lambda () ,@head)))))))

(defun jquery-libs ()
  (<:link :rel "stylesheet" :type "text/css" :href "res/css/lib/smoothness/jquery-ui-1.8.14.custom.css")
  (<:link :rel "stylesheet" :type "text/css" :href "res/css/lib/chosen.css")
  (<:script :type "text/javascript" :src "res/js/lib/jquery-1.5.1.min.js")
  (<:script :type "text/javascript" :src "res/js/lib/chosen.jquery.min.js")
  (<:script :type "text/javascript" :src "res/js/lib/jquery-ui-1.8.14.custom.min.js"))

(defun logout-button ()
  (<:form :class "logout-button" :action "/logout" :method "post"
          (<:submit :class "btn" :value "Log Out")))

(defvar *errors* nil)
(defun error-messages ()
  (when *errors*
    (<:ul :class "errors"
          (mapc (lambda (err) (<:li (<:ah err))) *errors*))))

(defun text-input-field (name label &key (type "text") max-length value error)
  (<:div :class "field"
    (<:label :for name (<:ah label))
    (<:input :name name :id name :type type :maxlength max-length
             :value value)
    (when error
      (<:span :class "error" (<:ah error)))))

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
(defpage index () ()
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
          (text-input-field "email" "Email")
          (text-input-field "password" "Password" :type "password")
          (<:submit :value "Log in")))

;;; /stage
(defpage stage (session-token char-name) ((gameplay-js-libs))
    "All the World's a Stage"
  (chat-area char-name)
  (<:p :style "visibility:hidden;overflow:hidden;" :id "wstoken" (<:ah session-token))
  #+nil
  (scene-recording)
  #+nil
  (scene-list-link)
  #+nil
  (logout-button))

(defun gameplay-js-libs ()
  ;; When you feel like figuring out why optional loading fails, take the following
  ;; two lines out...
  (<:script :type "text/javascript" :src "res/js/lib/swfobject.js")
  (<:script :type "text/javascript" :src "res/js/lib/web_socket.js")
  (<:script :type "text/javascript" :src "res/js/lib/json2.js")
  (<:script :type "text/javascript" :src "res/js/client.js"))

(defun chat-area (char-name)
  (<:div :id "main-game-panel"
         (game-panel char-name)
         (ooc-panel)))

(defun game-panel (char-name)
  (<:div :id "game-panel"
         (<:div :class "scene-display" :id "scene-display"
                (<:img :src "res/loading.gif" :id "loading-spinner"))
         (<:div :id "game-input"
                (<:ul
                 (<:li (<:href "#dialogue-tab" "Dialogue"))
                 (<:li (<:href "#action-tab" "Action"))
                 (<:li (<:href "#ooc-tab" "OOC"))
                 (<:li (<:href "#emit-tab" "Emit")))
                (<:div :id "dialogue-tab"
                       (<:form :id "dialogue-input"
                               (<:div
                                (<:p :class "character" (<:ah (string-capitalize char-name)))
                                (<:textarea :class "dialogue"))))
                (<:div :id "action-tab"
                       (<:form :id "action-input"
                               (<:ah "What do you do? ")
                               (<:input)))
                (<:div :id "ooc-tab"
                       (<:form :id "ooc-input"
                               "OOC - " (<:input)))
                (<:div :id "emit-tab"
                       (<:form :id "emit-input"
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
   (loop
      for i from 0
      for name in characters
      do (<:li (character-link name i)))))

(defun character-link (char idx)
  (<:href (format nil "/stage?char=~A" idx)
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
(defun scene (id)
  ;; TODO - THIS IS DANGEROUS AND LEAKY. XSS GALORE.
  (<:script :type "text/javascript" :src "res/js/client.js")
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
         (sykosomatic.scene::find-scene-entries id))))

;;; /signup
(defpage signup (form) ()
    "Sign up"
  (error-messages)
  (signup-component form))

(defun signup-component (form)
  (<:form :name "signup" :action "/signup" :method "post"
          (<:fieldset
           (<:legend "Sign up!")
           (text-input-field "email" "Email"
                             :value (field-raw-value form :email)
                             :error (field-error form :email))
           (text-input-field "display-name" "Display Name"
                             :max-length 32
                             :value (field-raw-value form :display-name)
                             :error (field-error form :display-name))
           (text-input-field "password" "Password" :type "password"
                             :error (field-error form :password))
           (text-input-field "confirmation" "Confirm password" :type "password"
                             :error (field-error form :confirmation)))
          (<:submit :value "Submit")))
