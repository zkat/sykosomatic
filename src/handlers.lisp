(cl:in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-handler* '404-handler)
  (pushnew 404 *approved-return-codes*)
  (setf *session-removal-hook* 'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  (setf *catch-errors-p* nil))

(defun teardown-hunchentoot ()
  (when *server* (stop *server*) (setf *server* nil)))

(defun logout (session)
  (let ((account-name (session-value 'account-name session))
        (websocket-clients (session-websocket-clients session)))
    (when account-name
      (format t "~&~A logged out.~%" account-name))
    (when websocket-clients
      (mapc #'disconnect-client websocket-clients))))

(defun session-cleanup (session)
  (format t "~&Session timed out. Trying to log it out...~%")
  (logout session))

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
          (<:text :name "name")
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
          (<:text :name "account-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:submit :value "Submit")))

(defun render-chat-box ()
  (<:div :class "chat-box" :id "chat-box"
         (<:div :class "sceneheader-div"
                (<:p :class "sceneheader"
                     (<:ah "int. breakfast bar. night."))
                (<:div :class "user-entry"
                       (<:p :class "action"
                            (<:ah "The entire 'bar' is only about 7 feed wide, with just enough depth and
height to hold the bar, with stovetops behind it, and the 6 barstools. It is lit by a few dingy
bulbs. The walls on either side are covered with tiny scraps of paper, old posters, and splatters of
what used to be food.  This bar is literally an alley, and beneath the detritus you know the walls
are actually the exteriors of two buildings.")))
                (<:div :class "user-entry"
                       (<:p :class "action"
                            (<:ah "The smell of bacon. pancakes, and syrup is overpowering."))))))

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

(defun render-scene-list-link ()
  (<:href "/scenes" "My Scenes"))

(defun render-scene-list ()
  (<:ul :class "scene-list"
   (mapc (lambda (scene &aux (id (scene-id scene)))
           (<:li (<:href (format nil "/view-scene?id=~A" id) (<:ah id))))
         (find-scenes-by-account-name (current-account-name)))))

(defun render-user-action (user-action)
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

(defun render-scene (id)
  (<:div :class "chat-box" :id "chat-box"
   (mapc (lambda (action-obj)
           (render-user-action (make-user-action :user (jsown:val action-obj "character")
                                                 :action (jsown:val action-obj "action")
                                                 :dialogue (jsown:val action-obj "dialogue"))))
         (find-scene-actions id))))

(defun render-scene-rating (scene-id)
  (<:p (<:ah "Rating: " (scene-rating scene-id))))

(defun render-scene-upvote (scene-id)
  (<:form :method "post" :action (format nil "/view-scene?id=~A" scene-id)
          (<:submit :value "Vote for Scene")))

;;;
;;; Handlers
;;;

(defun current-account-name (&optional (*session* *session*))
  (session-value 'account-name))

(defun 404-handler ()
  (render-page "404 Not Found"
               (lambda ()
                 (setf (return-code*) +http-not-found+)
                 (<:p (<:ah "Sorry, that page does not exist.")))))

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
                      (render-scene-list-link)
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

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (render-page "My scenes" #'render-scene-list))

(define-easy-handler (view-scene :uri "/view-scene") (id)
  (case (request-method*)
    (:get
     (render-page "Viewing Scene"
                  (lambda ()
                    (render-scene id)
                    (when (session-value 'account-name)
                      (render-scene-upvote id))
                    (when (scene-rating id)
                      (render-scene-rating id)))))
    (:post
     (ensure-logged-in)
     (scene-upvote id (session-value 'account-name))
     (redirect (format nil "/view-scene?id=~A" id)))))

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
