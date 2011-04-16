(cl:defpackage #:sykosomatic
  (:use #:cl #:alexandria #:hunchentoot #:yaclml #:sykosomatic.account #:sykosomatic.character
        #:sykosomatic.scene)
  (:import-from #:sykosomatic.db #:init-db))
(cl:in-package #:sykosomatic)

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port*
  ;; 843 ; makes flash load faster, but can only do this as root.
  8889
)
(defvar *current-story* nil)
(defvar *max-action-id* 0)
(defparameter *sykosomatic-path* (asdf:system-relative-pathname 'sykosomatic "res/"))
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

(defun render-user-action-to-json (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action))
        (user (user-action-user user-action)))
    (jsown:to-json
     `("user-action"
       (:obj ("action" . ,action)
             ("character" . ,user)
             ("dialogue" . ,dialogue))))))

(defun session-websocket-clients (session)
  (session-value 'websocket-clients session))
(defun (setf session-websocket-clients) (new-value session)
  (setf (session-value 'websocket-clients session) new-value))

;; clws stuff
(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric validate-client (chat-server client))

(defun disconnect-client (client)
  (let ((ws-client (client-ws-client client)))
    (ws:write-to-client ws-client :close)
    ;; Chrome doesn't seem to pay attention to :close.
    ;; _3b proposes sending a custom 'close' command to Chrome, and closing the socket client-side.
    (ws::client-disconnect ws-client :abort t)
    (when (client-session client)
      (deletef (session-websocket-clients session)
               client))))

(defstruct client uri host headers user-agent ws-client session account-name character-id)

(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test #'eq))))

(defun find-client (chat-server ws-client)
  (gethash ws-client (slot-value chat-server 'clients)))

(defmethod add-client ((srv chat-server) client)
  (format t "~&Adding Pending Client ~A.~%" client)
  (setf (gethash (client-ws-client client) (slot-value srv 'clients))
        client))

(defmethod remove-client ((srv chat-server) client)
  (remhash (client-ws-client client) (slot-value srv 'clients)))

(defmethod validate-client ((srv chat-server) client &aux (*acceptor* *server*))
  (let ((session (session-verify (make-instance 'request
                                                :uri (client-uri client)
                                                :remote-addr (client-host client)
                                                :headers-in (client-headers client)
                                                :acceptor *server*))))
    (when (and session (session-value 'account-name session))
      (setf (client-session client) session
            (client-account-name client) (session-value 'account-name session))
      client)))

(defun process-client-validation (res client json-message
                                  &aux (*acceptor* *server*)
                                  (message (jsown:parse json-message)))
  (push (cons :user-agent (jsown:val message "useragent"))
        (client-headers client))
  (let ((character (find-character (jsown:val message "char"))))
    (if (and (validate-client res client)
             character
             (string-equal (character-account-name character)
                           (client-account-name client)))
        (progn
          (format t "~&Client validated: ~A. ~%It's now playing as ~A.~%"
                  client (character-name character))
          (setf (client-character-id client) (character-id character))
          (push (session-websocket-clients (client-session client)) client))
        (progn
          (format t "~&No session. Disconnecting client. (~A)~%" client)
          (disconnect-client client)))))

(defun register-chat-server ()
  (ws:register-global-resource
   "/chat"
   (make-instance 'chat-server)
   #'ws::any-origin))

(defmethod ws:resource-accept-connection ((res chat-server) resource-name headers ws-client)
  (format t "~&Got client connection.~%")
  (let (alist-headers)
    (maphash (lambda (k v)
               (push (cons (intern (string-upcase k) :keyword)
                           v)
                     alist-headers))
             headers)
    (add-client res (make-client :uri resource-name :headers alist-headers :ws-client ws-client)))
  t)

(defmethod ws:resource-client-disconnected ((res chat-server) ws-client
                                            &aux (client (find-client res ws-client)))
  (format t "~&Client ~A disconnected.~%" client)
  (remove-client res client))

(defmethod ws:resource-received-frame ((res chat-server) ws-client message
                                       &aux (client (find-client res ws-client)))
  (format t "~&Received resource frame.~%")
  (if (client-session client)
      (process-client-message res client message)
      (process-client-validation res client message)))

(defun client-character-name (client)
  (when-let ((character (find-character-by-id (client-character-id client))))
    (character-name character)))

(defparameter *dispatch*
  '(("user-input" . process-user-input)
    ("ping" . process-ping)
    ("start-recording"  . start-recording)
    ("stop-recording" . stop-recording)
    ("char-desc" . get-character-description)))

(defun get-character-description (res client charname)
  (declare (ignore res))
  (format t "~&Got a character description request: ~S.~%" charname)
  (ws:write-to-client (client-ws-client client)
                      (jsown:to-json (list "char-desc" (character-description (find-character charname))))))

(defun save-user-action (scene-id user-action)
  (format t "~&Saving user action ~A under scene-id ~A~%" user-action scene-id)
  (add-action scene-id
              :character (user-action-user user-action)
              :action (user-action-action user-action)
              :dialogue (user-action-dialogue user-action)
              :timestamp (user-action-timestamp user-action)))

(defun send-user-action (client user-action)
  (when-let ((scene-id (session-value 'scene-id (client-session client))))
    (save-user-action scene-id user-action))
  (ws:write-to-client (client-ws-client client)
                      (render-user-action-to-json user-action)
                      #+nil(jsown:to-json
                       (list "user-action"
                             (with-yaclml-output-to-string
                               (render-user-action user-action))))))

(defun process-user-input (res client action dialogue)
  (maphash-values (rcurry #'send-user-action (add-user-action (client-character-name client) action dialogue))
                  (slot-value res 'clients)))

(defun process-ping (res client)
  (declare (ignore res))
  (ws:write-to-client (client-ws-client client) (jsown:to-json (list "pong"))))

(defun start-recording (res client)
  (declare (ignore res))
  (format t "~&Request to start recording received.~%")
  (if (session-value 'scene-id (client-session client))
      (format t "~&Scene already being recorded. Ignoring request.~%")
      (let ((*acceptor* *server*))
        (setf (session-value 'scene-id (client-session client))
              (create-scene (client-account-name client))))))

(defun stop-recording (res client)
  (declare (ignore res))
  (format t "~&Request to stop recording received.~%")
  (delete-session-value 'scene-id (client-session client)))

(defun process-client-message (res client raw-message &aux (message (jsown:parse raw-message)))
  (let ((action (cdr (assoc (car message) *dispatch* :test #'string=))))
    (when action
      (apply action res client (cdr message)))))

;; Server startup/teardown.
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

(defun begin-shared-hallucination ()
  ;; Cleanup
  (when *server* (end-shared-hallucination) (warn "Restarting server."))
  ;; Database
  (init-db)
  ;; Websockets
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
  ;; Hunchentoot
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-handler* '404-handler)
  (pushnew 404 *approved-return-codes*)
  (setf *session-removal-hook* 'session-cleanup)
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
