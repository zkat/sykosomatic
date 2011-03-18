(cl:in-package #:belletrist)

;; Ideas:

;; Story:
;; In general, players can hang around and play casually however they want. If/when they have an
;; idea or want to play out something more seriously, they can choose to start a 'scene' to earn
;; points.

;; A scene will be recorded, and transcribed to look like a screenplay once it's done. It will then
;; be posted in a community area, where others can rate the overall scene, as well as individual
;; characters in it.

;; Scene scores are divided amongst players, and individual achievement scores give individual
;; players bonuses. Points are awarded based on score, and the points can be used towards something.

;; Templating:

;; All templating systems suck. This one won't. No logic goes into a template, and templates should
;; be kept small (like functions). Additionally, multiple templates will be kept in a single file,
;; which will generate either lisp functions or CLOS objects to correspond to each individual
;; template, which the programmer can compose with the logic (much like pages are strung together
;; with logic). The template files will basically look like lisp files, and require parameter
;; declaration for clarity, as well as accept an optional docstring.
;;
;; Example:
;; (deftempl standard-page (title head-contents body-contents)
;;   "This template renders the standard page thingy."
;;   <html>
;;     <head>
;;       <title>{title}</title>
;;       {head}
;;     </head>
;;     <body>
;;     {body-contents}
;;     </body>
;;   </html>)
;;
;; The above can be loaded (probably with a special reader macro), and could then create a function
;; to be called on a stream, with the required parameters:
;; (load "page.templ")
;; (render-template 'standard-page *standard-output* :title "My special page" :body-contents "<p>Hello, World!</p>")
;;
;; Question: Do even minor instances of HTML need to be templated out?

;; TODO for today:
;; * group actions and dialog by user.
;; * Use (CONT'D.)

;; WebSockets
;; * Figure out how to validate http sessions vs websocket connections.
;; * Handle user input through websockets.

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port* 12345)
(defparameter *current-story* nil)
(defvar *users* nil)
(defvar *max-action-id* 0)
(defvar *folder-dispatcher-pushed-p* nil)
(defparameter *belletrist-path* (asdf:system-relative-pathname 'belletrist "res/"))

(defstruct user-action id user timestamp action dialogue)

(defun add-user-action (user action dialogue &optional (timestamp (get-universal-time))
                         &aux (id (incf *max-action-id*)))
  (push (make-user-action :id id :user user :timestamp timestamp :action action :dialogue dialogue)
        *current-story*)
  id)

(defun get-recent-actions (last-action-id)
  (member (1+ last-action-id) (reverse *current-story*) :key #'user-action-id))

(defun render-user-action (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action)))
    (<:div :class "user-entry"
      (if (and (not (emptyp action)) (emptyp dialogue))
          (<:p :class "action"
               (<:ah action))
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

;; clws stuff
;; TODO - Check all this against HT session.
(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric find-client (chat-server host port))
(defun client-key (client)
  (cons (ws:client-host client) (ws:client-port client)))
(defun validating-add-client (chat-server client)
  (add-client chat-server client))

(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test 'equal))))
(defmethod add-client ((srv chat-server) client)
  (setf (gethash (client-key client) (slot-value srv 'clients))
        client))
(defmethod remove-client ((srv chat-server) client)
  (remhash (client-key client) (slot-value srv 'clients)))
(defmethod find-client ((srv chat-server) host port)
  (gethash (client-key client) (slot-value srv 'clients)))

(defun register-chat-server ()
  (ws:register-global-resource
   "/chat"
   (make-instance 'chat-server)
   #'ws::any-origin))

(defmethod resource-accept-connection ((res chat-server) resource-name headers client)
  (declare (ignore resource-name headers))
  (validating-add-client res client))

(defmethod resource-client-disconnectod ((res chat-server) client message &aux (client-key (client-key client)))
  (format t "~&Client @~A:~A disconnected: ~A~%" (car client-key) (cdr client-key) message)
  (remove-client res client))

(defmethod resource-receive-frame ((res chat-server) client message)
  (process-client-message client message))

(defun process-client-message (client message)
  (format t "~&~A sez: ~S~%" client message))

;; Handlers
(defun logout (username)
  (format t "~&~A logged out.~%" username)
  (deletef *users* username :test #'string-equal))

(define-easy-handler (login :uri "/login") (username)
  (if (and *session* (session-value 'username))
      (redirect "/")
      (start-session))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Login Page")
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
     (<:body
      (if (and username (not (find username *users* :test #'string-equal)))
          (progn
            (<:p (<:ah "Successfully logged in as " username "."))
            (push username *users*)
            (setf (session-value 'username) username)
            (format t "~&~A logged in.~%" username)
            (redirect "/"))
          (<:div
           (<:form :name "username" :action "/login"
            (<:label (<:ah "Pick a username: "))
            (<:input :type "text" :name "username")
            (<:input :type "submit" :value "Submit"))
           (when username
             (<:label (<:ah "Sorry, that username is already being used.")))))))))

(define-easy-handler (home :uri "/") ()
  (unless (and *session* (session-value 'username))
    (redirect "/login"))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Hello!")
      (<:link :rel "stylesheet" :type "text/css" :href "res/styles.css")
      (<:script  :type "text/javascript" :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js")
      (<:script :type "text/javascript" :src "res/ajaxlib.js")
      (<:script :type "text/javascript" :src "res/swfobject.js")
      (<:script :type "text/javascript" :src "res/web_socket.js"))
     (<:body :onload "init();"
      (<:div :class "chat-box" :id "chat-box" (<:div :class "sceneheader-div"
                                                     (<:p :class "sceneheader"
                                                          (<:ah "ext. josh's computer. night."))))
      (<:form :class "user-story" :name "user-story" :action "javascript:addMsg()"
       (<:label (<:ah "Action: "))
       (<:input :type "textarea" :id "user-action")
       (<:label (<:ah "Dialogue: "))
       (<:input :type "textarea" :id "user-dialogue")
       (<:input :type "submit" :value "Send"))
      (<:form :class "logout-button" :action "/logout"
        (<:input :type "submit" :value "Log Out"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (session-value 'username))
    (let ((username (session-value 'username)))
      (logout username)
      (setf (session-value 'username) nil)))
  (redirect "/login"))

;; Server startup/teardown.
(defun session-cleanup (session)
  (let ((username (session-value 'username session)))
    (when username
      (logout username))))

(defun begin-shared-hallucination ()
  (unless *folder-dispatcher-pushed-p*
    (push (create-folder-dispatcher-and-handler
           "/res/" *belletrist-path*)
          *dispatch-table*))
  (register-chat-server)
  (bordeaux-threads:make-thread
   (lambda ()
     (ws:run-server *chat-server-port*))
   :name "websockets server")
  (bt:make-thread
   (lambda () (ws:run-resource-listener (ws:find-global-resource "/chat")))
   :name "chat resource listener")
  (when *server* (end-shared-hallucination) (warn "Restarting server."))
  (setf *users* nil
        *session-removal-hook* #'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))
