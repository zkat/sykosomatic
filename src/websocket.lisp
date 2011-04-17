(in-package :sykosomatic)

(defstruct (user-action (:constructor make-user-action (user action dialogue
                                                        &optional (timestamp (get-universal-time)))))
  user timestamp action dialogue)

(defun render-user-action-to-json (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action))
        (user (user-action-user user-action)))
    (jsown:to-json
     `("user-action"
       (:obj ("action" . ,action)
             ("character" . ,user)
             ("dialogue" . ,dialogue))))))

;;;
;;; Generic server code
;;;
(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric validate-client (chat-server client))
(defgeneric disconnect-client (chat-server client))

(defstruct client server uri host headers user-agent ws-client session account-name character-id)

(defun client-write (client string)
  (ws:write-to-client (client-ws-client client) string))

(defun find-client (chat-server ws-client)
  (gethash ws-client (slot-value chat-server 'clients)))

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
          (logit "Client validated: ~A. It's now playing as ~A."
                  client (character-name character))
          (setf (client-character-id client) (character-id character))
          (push (session-websocket-clients (client-session client)) client))
        (progn
          (logit "No session. Disconnecting client. (~A)" client)
          (disconnect-client res client)))))

(defun client-character-name (client)
  (when-let ((character (find-character-by-id (client-character-id client))))
    (character-name character)))

;;;
;;; CLWS-based server.
;;;
(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test #'eq))))

(defmethod disconnect-client ((server chat-server) client)
  (let ((ws-client (client-ws-client client))
        (session (client-session client)))
    (ws:write-to-client ws-client :close)
    ;; Chrome doesn't seem to pay attention to :close.
    (ws::client-disconnect ws-client :abort t)
    (when session
      (deletef (session-websocket-clients session)
               client))))

(defun register-chat-server ()
  (ws:register-global-resource
   "/chat"
   (make-instance 'chat-server)
   #'ws::any-origin))

(defmethod add-client ((srv chat-server) client)
  (logit "Adding Pending Client ~A." client)
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

(defmethod ws:resource-accept-connection ((res chat-server) resource-name headers ws-client)
  (logit "Got client connection.")
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
  (logit "Client ~A disconnected." client)
  (remove-client res client))

(defmethod ws:resource-received-frame ((res chat-server) ws-client message
                                       &aux (client (find-client res ws-client)))
  (logit "Received resource frame.")
  (if (client-session client)
      (process-client-message res client message)
      (process-client-validation res client message)))

;;;
;;; Client messages
;;;
(defparameter *dispatch*
  '(("user-input" . process-user-input)
    ("ping" . process-ping)
    ("start-recording"  . start-recording)
    ("stop-recording" . stop-recording)
    ("char-desc" . get-character-description)))

(defun process-client-message (res client raw-message &aux (message (jsown:parse raw-message)))
  (when-let ((action (cdr (assoc (car message) *dispatch* :test #'string=))))
    (apply action res client (cdr message))))

(defun get-character-description (res client charname)
  (declare (ignore res))
  (logit "Got a character description request: ~S." charname)
  (client-write client (jsown:to-json (list "char-desc" (character-description (find-character charname))))))

(defun save-user-action (scene-id user-action)
  (logit "Saving user action ~A under scene-id ~A" user-action scene-id)
  (add-action scene-id
              :character (user-action-user user-action)
              :action (user-action-action user-action)
              :dialogue (user-action-dialogue user-action)
              :timestamp (user-action-timestamp user-action)))

(defun send-user-action (client user-action)
  (when-let ((scene-id (session-value 'scene-id (client-session client))))
    (save-user-action scene-id user-action))
  (client-write client (render-user-action-to-json user-action)))

(defun process-user-input (res client dialogue)
  (maphash-values (rcurry #'send-user-action (make-user-action (client-character-name client) nil dialogue))
                  (slot-value res 'clients)))

(defun process-ping (res client)
  (declare (ignore res))
  (client-write client (jsown:to-json (list "pong"))))

(defun start-recording (res client)
  (declare (ignore res))
  (logit "Request to start recording received.")
  (if (session-value 'scene-id (client-session client))
      (logit "Scene already being recorded. Ignoring request.")
      (let ((*acceptor* *server*))
        (setf (session-value 'scene-id (client-session client))
              (create-scene (client-account-name client))))))

(defun stop-recording (res client)
  (declare (ignore res))
  (logit "Request to stop recording received.")
  (delete-session-value 'scene-id (client-session client)))

;;;
;;; Init/teardown
;;;
(defvar *websocket-thread* nil)
(defvar *chat-resource-thread* nil)

(defun init-websockets (&optional (port *chat-server-port*))
  (register-chat-server)
  (setf *websocket-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (ws:run-server port))
         :name "websockets server"))
  (setf *chat-resource-thread*
        (bt:make-thread
         (lambda () (ws:run-resource-listener (ws:find-global-resource "/chat")))
         :name "chat resource listener")))

(defun teardown-websockets ()
  (when (and *websocket-thread* (bt:thread-alive-p *websocket-thread*))
    (bt:destroy-thread *websocket-thread*)
    (setf *websocket-thread* nil))
  (when (and *chat-resource-thread* (bt:thread-alive-p *chat-resource-thread*))
    (bt:destroy-thread *chat-resource-thread*)
    (setf *chat-resource-thread* nil)))
