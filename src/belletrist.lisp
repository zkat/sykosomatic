(cl:defpackage #:belletrist
  (:use #:cl #:alexandria #:hunchentoot #:yaclml #:belletrist.account))
(cl:in-package #:belletrist)

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port*
  ;; 843 ; makes flash load faster, but can only do this as root.
  8889
)
(defvar *current-story* nil)
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
  ;; _3b proposes sending a custom 'close' command to Chrome, and closing the socket client-side.
  (ws::client-disconnect client :abort t))

(defun client-session (client)
  (cdr (find client (session-db *server*) :key (compose (curry #'session-value 'websocket-client) #'cdr))))

(defun client-account-name (client &aux (session (client-session client)))
  (when session
    (session-value 'account-name session)))

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
  (declare (ignore res))
  (let* ((action-obj (jsown:parse message))
         (action (cdr (assoc "action" (cdr action-obj) :test #'string=)))
         (dialogue (cdr (assoc "dialogue" (cdr action-obj) :test #'string=)))
         (user-action (add-user-action (client-account-name client) action dialogue)))
    (loop for (nil . session) in (session-db *server*)
       for c = (session-value 'websocket-client session)
       when c
       do (ws:write-to-client c (with-yaclml-output-to-string
                                  (render-user-action user-action))))))

;; Handlers
(defun logout (session)
  (let ((account-name (session-value 'account-name session))
        (websocket-client (session-value 'websocket-client session)))
    (when account-name
      (setf (session-value 'account-name session) nil)
      (format t "~&~A logged out.~%" account-name))
    (when websocket-client
      (disconnect-client websocket-client))))

(defun active-account-sessions (account-name)
  "Finds all sessions that are logged in as ACCOUNT-NAME."
  (loop for (nil . session) in (session-db *server*)
     for session-user = (session-value 'account-name session)
     when (and session-user (string-equal session-user account-name))
     collect session))

(defun render-signup-component ()
  (<:form :name "signup" :action "/signup"
          (<:label (<:ah "Sign up:"))
          (<:br)
          (<:label (<:ah "Email"))
          (<:input :type "text" :name "account-name")
          (<:br)
          (<:label (<:ah "Password"))
          (<:input :type "password" :name "password")
          (<:br)
          (<:label (<:ah "Confirm password"))
          (<:input :type "password" :name "confirmation")
          (<:br)
          (<:input :type "submit" :value "Submit")))

(defun render-error-messages (errors)
  (<:ul :class "errorlist"
        (mapc (lambda (err) (<:li (<:ah err))) errors)))

(define-easy-handler (signup :uri "/signup") (account-name password confirmation)
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Sign up for Belletrist.")
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
     (<:body
      (if (emptyp account-name)
          (render-signup-component)
          (multiple-value-bind (account-created-p errors)
              (create-account account-name password confirmation)
            (if account-created-p
                (progn
                  (format t "~&Account created: ~A~%" account-name)
                  (redirect "/login"))
                (progn
                  (render-error-messages errors)
                  (render-signup-component)))))))))

(defun render-login-component ()
  (<:form :name "login" :action "/login"
          (<:label (<:ah "Log in:"))
          (<:input :type "text" :name "account-name")
          (<:input :type "password" :name "password")
          (<:input :type "submit" :value "Submit")))

(define-easy-handler (login :uri "/login") (account-name password)
  (if (and *session* (session-value 'account-name))
      (redirect "/")
      (start-session))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Login Page")
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
     (<:body
      (if account-name
          (if-let ((account (validate-credentials account-name password)))
            (progn
              (when-let ((other-logins (active-account-sessions account-name)))
                (mapcar #'logout other-logins))
              (setf (session-value 'account-name) account-name)
              (format t "~&~A logged in.~%" account-name)
              (<:p (<:ah "Successfully logged in as " account-name "."))
              (redirect "/"))
            (<:div (<:p :class "error-msg" "Invalid credentials. Login failed.")
                   (render-login-component)))
          (render-login-component))
      (<:a :href "/signup" "Create account.")))))

(define-easy-handler (home :uri "/") ()
  (unless (and *session* (session-value 'account-name))
    (redirect "/login"))
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
  (when (and *session* (session-value 'account-name))
    (logout *session*))
  (redirect "/login"))

(define-easy-handler (ajax-ping :uri "/pingme") ()
  (unless (and *session* (session-value 'account-name))
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
  (setf *session-removal-hook* #'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  (setf *catch-errors-p* nil)
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil))
  (when (and *websocket-thread* (bt:thread-alive-p *websocket-thread*))
    (bt:destroy-thread *websocket-thread*)
    (setf *websocket-thread* nil))
  (when (and *chat-resource-thread* (bt:thread-alive-p *chat-resource-thread*))
    (bt:destroy-thread *chat-resource-thread*)
    (setf *chat-resource-thread* nil)))
