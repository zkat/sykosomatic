(cl:in-package :sykosomatic)

;;;
;;; Utils
;;;

(defun logout (session)
  (let ((account-name (session-value 'websocket-client session))
        (websocket-client (session-value 'websocket-client session)))
    (when account-name
      (format t "~&~A logged out.~%" account-name))
    (when websocket-client
      (disconnect-client websocket-client))))

(defun ensure-logged-in ()
  (unless (and *session* (session-value 'account-name))
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
          (mapc (lambda (err) (<:li (<:ah err))) errors))))

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

(defun render-character-link (char-name)
  (<:a :href (format nil "/?char=~A" char-name) (<:ah char-name)))

(defun render-character-selection ()
  (<:ul
   (mapc (lambda (char) (<:li (render-character-link char)))
         nil
         #+nil(list-user-character-names (session-value 'account-name)))))

;;;
;;; Handlers
;;;

;;; Main page
(define-easy-handler (home :uri "/") (#+nil char)
  (ensure-logged-in)
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Hello!")
      (<:link :rel "stylesheet" :type "text/css" :href "res/styles.css")
      (<:script  :type "text/javascript" :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js")
      ;; When you feel like figuring out why optional loading fails, take the following two lines out...
      (<:script :type "text/javascript" :src "res/swfobject.js")
      (<:script :type "text/javascript" :src "res/web_socket.js")
      (<:script :type "text/javascript" :src "res/ajaxlib.js"))
     (<:body
      (render-chat-box)
      (render-user-input-area)
      (render-logout-button)))))

;;; Login/logout
(define-easy-handler (login :uri "/login") (account-name password)
  (unless *session*
    (start-session))
  (case (request-method*)
    (:get
     (with-yaclml-output-to-string
       (<:html
        (<:head
         (<:title "Login Page")
         (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
        (<:body
         (when-let ((account-name (session-value 'account-name)))
           (push (format nil "Already logged in as ~A." account-name)
                 (session-value 'errors)))
         (render-error-messages)
         (render-login-component))
        (<:a :href "/signup" "Create account."))))
    (:post
     (if-let ((account (validate-credentials account-name password)))
       (progn
         (setf (session-value 'account-name) (account-name account)
               (session-value 'display-name) (account-display-name account))
         (format t "~&~A logged in.~%" account-name)
         (redirect "/"))
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
     (with-yaclml-output-to-string
       (<:html
        (<:head
         (<:title "Sign up for Sykosomatic")
         (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
        (<:body
         (render-error-messages)
         (render-signup-component)))))))

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
             (redirect "/"))
           (progn
             (appendf (session-value 'errors) errors)
             (redirect "/signup")))))
    (:get
     (with-yaclml-output-to-string
       (<:html
        (<:head
         (<:title "Character creation")
         (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
        (<:body
         (render-error-messages)
         (render-character-creation-component)))))))

(define-easy-handler (character-selection :uri "/charselect") ()
  (ensure-logged-in)
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Character Selection")
      (<:script  :type "text/javascript" :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"))
     (<:body
      (render-character-selection)
      (render-logout-button)))))

;;; Misc
(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in))
