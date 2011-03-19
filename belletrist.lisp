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
;; * Account creation.
;; * Persistent accounts.
;; * Persistent scenes.
;; * OOC message pane.

;; TODO later:
;; * group actions and dialog by user.
;; * Use (CONT'D.)
;; * Instead of alerts, replace the chat box with 'loading...' until the websocket is connected,
;;   then show the whole chat box.
;; * Fix clws to work on CCL.

;; clws issues
;; * Doesn't work on CCL.
;; * client accessors aren't exported.
;; * No SSL support.

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port* 8889)
(defvar *current-story* nil)
(defvar *users* nil)
(defvar *max-action-id* 0)
(defvar *folder-dispatcher-pushed-p* nil)
(defparameter *belletrist-path* (asdf:system-relative-pathname 'belletrist "res/"))
(defvar *websocket-thread* nil)
(defvar *chat-resource-thread* nil)

(defstruct user-action id user timestamp action dialogue)

(defun add-user-action (user action dialogue &optional (timestamp (get-universal-time))
                         &aux (id (incf *max-action-id*)))
  (let ((user-action (make-user-action :id id :user user :timestamp timestamp :action action :dialogue dialogue)))
    (push user-action *current-story*)
    user-action))

(defun get-recent-actions (last-action-id)
  (member (1+ last-action-id) (reverse *current-story*) :key #'user-action-id))

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

;; clws stuff
(defgeneric add-client (chat-server client resource-name header))
(defgeneric remove-client (chat-server client))
(defgeneric validate-client (chat-server client user-agent))

(defun disconnect-client (client)
  (ws:write-to-client client :close)
  ;; Chrome doesn't seem to pay attention to :close.
  (ws::client-disconnect client :abort t))

(defun client-session (client)
  (cdr (find client (session-db *server*) :key (compose (curry #'session-value 'websocket-client) #'cdr))))

(defun client-username (client &aux (session (client-session client)))
  (when session
    (session-value 'username session)))

(defclass chat-server (ws:ws-resource)
  ;; Really just 'pending' clients.
  ((clients :initform nil)))

(defmethod add-client ((srv chat-server) client resource-name headers)
  (format t "~&Adding Pending Client ~A.~%Resource name: ~A~%Headers: ~S~%"
          client resource-name headers)
  (push (list client resource-name headers)
        (slot-value srv 'clients)))

(defmethod remove-client ((srv chat-server) client)
  (deletef (slot-value srv 'clients) client :key #'car))

(defmethod validate-client ((srv chat-server) client user-agent &aux (*acceptor* *server*))
  (let* ((params (cdr (assoc client (slot-value srv 'clients))))
         (resource-name (car params))
         (headers (cadr params))
         (_ (format t "~&Validating client ~A.~%Resource name: ~A~%Headers: ~S~%User Agent: ~A~%"
                    client resource-name headers user-agent))
         (req (make-instance 'request :uri resource-name :remote-addr (ws::client-host client)
                             :headers-in (cons (cons :user-agent user-agent) headers)
                             :acceptor *server*))
         (session (session-verify req)))
    (declare (ignore _))
    (if session
        (let ((old-session (cdr (find session (session-db *server*) :key #'cdr))))
          (format t "~&Got a session: ~A" session)
          (when old-session
            (let ((old-client (session-value 'websocket-client old-session)))
              (when old-client
                (format t "~&Found two clients using the same session. Disconnecting old one. (~A)~%" old-client)
                (disconnect-client old-client))))
          (remove-client srv client)
          (setf (session-value 'websocket-client session) client))
        (progn
          (format t "~&No session. Disconnecting client. (~A)~%" client)
          (disconnect-client client)))))

(defun register-chat-server ()
  (ws:register-global-resource
   "/chat"
   (make-instance 'chat-server)
   #'ws::any-origin))

(defmethod ws:resource-accept-connection ((res chat-server) resource-name headers client)
  (format t "~&Got client connection.~%")
  (add-client res client resource-name (let (alist-headers)
                                         (maphash (lambda (k v)
                                                    (push (cons (intern (string-upcase k) :keyword)
                                                                v)
                                                          alist-headers))
                                                  headers)
                                         alist-headers))
  t)

(defmethod ws:resource-client-disconnected ((res chat-server) client)
  (format t "~&Client ~A disconnected.~%" client)
  (remove-client res client))

(defmethod ws:resource-received-frame ((res chat-server) client message)
  (format t "~&Received resource frame.~%")
  (if (client-session client)
      (process-client-message res client message)
      (validate-client res client message)))

(defun process-client-message (res client message)
  (let* ((action-obj (jsown:parse message))
         (action (cdr (assoc "action" (cdr action-obj) :test #'string=)))
         (dialogue (cdr (assoc "dialogue" (cdr action-obj) :test #'string=)))
         (user-action (add-user-action (client-username client) action dialogue)))
    (loop for (nil . session) in (session-db *server*)
       for c = (session-value 'websocket-client session)
       when c
       do (ws:write-to-client c (with-yaclml-output-to-string
                                  (render-user-action user-action))))))

;; Handlers
(defun logout (session)
  (let ((username (session-value 'username session))
        (websocket-client (session-value 'websocket-clients session)))
    (when username
      (deletef *users* username :test #'string-equal)
      (format t "~&~A logged out.~%" username))
    (when websocket-client
      (disconnect-client websocket-client))))

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
      (<:script :type "text/javascript" :src "res/web_socket.js")
      (<:script :type "text/javascript" :src "res/ajaxlib.js"))
     (<:body
      (<:div :class "chat-box" :id "chat-box"
             (<:div :class "sceneheader-div"
                    (<:p :class "sceneheader"
                         (<:ah "int. josh's computer. night."))))
      (<:div :id "user-input"
       (<:form :class "user-story" :name "user-story" :action "javascript:addMsg()"
               (<:label (<:ah "Action: "))
               (<:input :type "textarea" :id "user-action")
               (<:label (<:ah "Dialogue: "))
               (<:input :type "textarea" :id "user-dialogue")
               (<:input :type "submit" :value "Send")))
      (<:form :class "logout-button" :action "/logout"
        (<:input :type "submit" :value "Log Out"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (session-value 'username))
    (let ((username (session-value 'username)))
      (logout username)
      (setf (session-value 'username) nil)))
  (redirect "/login"))

(define-easy-handler (ajax-ping :uri "/pingme") ()
  (unless (and *session* (session-value 'username))
    (redirect "/login")))

;; Server startup/teardown.
(defun session-cleanup (session)
  (logout session))

(defun begin-shared-hallucination ()
  (when *server* (end-shared-hallucination) (warn "Restarting server."))
  (unless *folder-dispatcher-pushed-p*
    (push (create-folder-dispatcher-and-handler
           "/res/" *belletrist-path*)
          *dispatch-table*))
  (register-chat-server)
  (setf *websocket-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (ws:run-server *chat-server-port*))
         :name "websockets server"))
  (setf *chat-resource-thread*
        (bt:make-thread
         (lambda () (ws:run-resource-listener (ws:find-global-resource "/chat")))
         :name "chat resource listener"))
  (setf *users* nil
        *session-removal-hook* #'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil))
  (when (and *websocket-thread* (bt:thread-alive-p *websocket-thread*))
    (bt:destroy-thread *websocket-thread*)
    (setf *websocket-thread* nil))
  (when (and *chat-resource-thread* (bt:thread-alive-p *chat-resource-thread*))
    (bt:destroy-thread *chat-resource-thread*)
    (setf *chat-resource-thread* nil)))
