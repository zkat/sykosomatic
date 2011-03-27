(cl:in-package :sykosomatic)

(declaim (optimize debug))
;;;
;;; Utils
;;;

(defun ensure-logged-in ()
  (unless (and *session* (session-value 'account-name))
    (push "You must be logged in to access that page."
          (session-value 'errors))
    (redirect "/login")))

(defun active-account-sessions (account-name)
  "Finds all sessions that are logged in as ACCOUNT-NAME."
  (loop for (nil . session) in (session-db *server*)
     for session-user = (session-value 'account-name session)
     when (and session-user (string-equal session-user account-name))
     collect session))

;;;
;;; Components
;;;
(defun render-signup-component ()
  (<:form :name "signup" :action "/signup" :method "post"
          (<:label (<:ah "Sign up:"))
          (<:br)
          (<:label (<:ah "Email"))
          (<:input :type "text" :name "account-name")
          (<:br)
          (<:label (<:ah "Display name"))
          (<:input :type "text" :name "display-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:label (<:ah "Confirm password"))
          (<:input :type "password" :name "confirmation")
          (<:br)
          (<:submit :value "Submit")))

(defun render-error-messages ()
  (when-let ((errors (session-value 'errors)))
    (<:ul :class "errorlist"
          (mapc (lambda (err) (<:li (<:ah err))) errors))
    (setf (session-value 'errors) nil)))

(defun render-character-creation-component ()
  (<:form :name "create-character" :action "/newchar" :method "post"
          (<:label (<:ah "Create a character."))
          (<:br)
          (<:label (<:ah "Name"))
          (<:input :type "text" :name "name")
          (<:br)
          (<:label (<:ah "Description"))
          (<:input :type "textfield" :name "description")
          (<:br)
          (<:submit :value "Submit")))

(defun render-login-component ()
  (<:form :name "login" :action "/login" :method "post"
          (<:label (<:ah "Log in"))
          (<:br)
          (<:label (<:ah "Email"))
          (<:input :type "text" :name "account-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:submit :value "Submit")))

(defun render-chat-box ()
  (<:div :class "chat-box" :id "chat-box"
         (<:div :class "sceneheader-div"
                (<:p :class "sceneheader"
                     (<:ah "int. josh's computer. night.")))))

(defun render-user-input-area ()
  (<:div :id "user-input"
         (<:form :class "user-story" :name "user-story" :action "javascript:addMsg()"
                 (<:label (<:ah "Action: "))
                 (<:input :type "textarea" :id "user-action")
                 (<:label (<:ah "Dialogue: "))
                 (<:input :type "textarea" :id "user-dialogue")
                 (<:submit :value "Send"))))

(defun render-logout-button ()
  (<:form :class "logout-button" :action "/logout" :method "post"
          (<:submit :value "Log Out")))

(defun render-character-link (char)
  (let ((name (character-name char)))
   (<:href (format nil "/stage?char=~A" (string-downcase name))
           (<:ah name))))

(defun render-character-list (characters)
  (print characters)
  (<:ul
   (mapc (lambda (char) (<:li (render-character-link char)))
         characters)))

(defun render-page (title body-fun &optional head-fun)
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title (<:ah title))
      (<:link :rel "stylesheet" :type "text/css" :href "res/styles.css")
      (<:script  :type "text/javascript" :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js")
      (when head-fun (funcall head-fun)))
     (<:body
      (funcall body-fun)))))

(defun render-gameplay-js-libs ()
  ;; When you feel like figuring out why optional loading fails, take the following
  ;; two lines out...
  (<:script :type "text/javascript" :src "res/swfobject.js")
  (<:script :type "text/javascript" :src "res/web_socket.js")
  (<:script :type "text/javascript" :src "res/json2.js")
  (<:script :type "text/javascript" :src "res/ajaxlib.js"))

(defun render-scene-recording ()
  (<:form :action "javascript:startRecording()"
          (<:submit :value "Start Recording"))
  (<:form :action "javascript:stopRecording()"
          (<:submit :value "Stop Recording")))

;;;
;;; Handlers
;;;

(defun current-account-name (&optional (*session* *session*))
  (session-value 'account-name))

;;; Main page
(define-easy-handler (home :uri "/") ()
  (render-page "Sykosomatic.org Dev Site"
               (lambda ()
                 (<:p (<:ah "Sykosomatic is a cooperative storytelling system, currently in development. ")
                      (<:href "/login" (<:ah "Log in.")))
                 (<:p (<:ah "Already logged in? What are you doing here?! Head to the ")
                      (<:href "/stage" (<:ah "Stage!"))))))

(define-easy-handler (play :uri "/stage") (char)
  (ensure-logged-in)
  (render-page "All the World's a Stage"
               (lambda ()
                 (if (emptyp char)
                     (progn
                       (push (format nil "You must select a character before playing.")
                             (session-value 'errors))
                       (redirect "/role"))
                     (progn
                      (render-chat-box)
                      (render-user-input-area)
                      (render-scene-recording)
                      (render-logout-button))))
               (lambda ()
                 (unless (emptyp char)
                   (render-gameplay-js-libs)))))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (render-page "What role shall you play?"
               (lambda ()
                 (render-error-messages)
                 (let ((characters (find-characters-by-account-name (session-value 'account-name))))
                   (render-character-list characters)
                   (<:href "/newchar" "Create a new character.")
                   (render-logout-button)))))

;;; Login/logout
(define-easy-handler (login :uri "/login") (account-name password)
  (unless *session*
    (start-session))
  (case (request-method*)
    (:get
     (render-page "Log in"
                  (lambda ()
                    (when-let ((account-name (session-value 'account-name)))
                      (push (format nil "Already logged in as ~A." account-name)
                            (session-value 'errors)))
                    (render-error-messages)
                    (render-login-component)
                    (<:href "/signup" (<:ah "Create account.")))))
    (:post
     (if-let ((account (validate-credentials account-name password)))
       (progn
         (setf (session-value 'account-name) (account-name account)
               (session-value 'display-name) (account-display-name account))
         (format t "~&~A logged in.~%" account-name)
         (redirect "/stage"))
       (progn
         (push "Invalid login or password." (session-value 'errors))
         (redirect "/login"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (session-value 'account-name))
    (logout *session*)
    (remove-session *session*))
  (redirect "/login"))

;;; Account creation and management
(define-easy-handler (signup :uri "/signup") (account-name display-name password confirmation)
  (case (request-method*)
    (:post
     (multiple-value-bind (account-created-p errors)
         (create-account account-name display-name password confirmation)
       (if account-created-p
           (progn
             (format t "~&Account created: ~A~%" account-name)
             (redirect "/login"))
           (progn
             (appendf (session-value 'errors) errors)
             (redirect "/signup")))))
    (:get
     (render-page "Sign up"
                  (lambda ()
                    (render-error-messages)
                    (render-signup-component))))))

;;; Characters
(define-easy-handler (newchar :uri "/newchar") (name description)
  (ensure-logged-in)
  (case (request-method*)
    (:post
     (multiple-value-bind (createdp errors)
         (create-character (session-value 'account-name) name description)
       (if createdp
           (progn
             (format t "~&Character created: ~A~%" name)
             (redirect "/stage"))
           (progn
             (appendf (session-value 'errors) errors)
             (redirect "/newchar")))))
    (:get
     (render-page "Create a character"
                  (lambda ()
                    (render-error-messages)
                    (render-character-creation-component))))))

;;; Misc
(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in)
  (setf (content-type*) "text/plain")
  "pong")
