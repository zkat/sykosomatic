(cl:defpackage #:sykosomatic.websocket
  (:use :cl :alexandria :hunchentoot
        :sykosomatic.util
        :sykosomatic.config
        :sykosomatic.session
        :sykosomatic.character
        :sykosomatic.account
        :sykosomatic.scene)
  (:export :init-websockets
           :teardown-websockets))
(cl:in-package #:sykosomatic.websocket)

(defun session-websocket-clients (session)
  (transient-session-value 'websocket-clients session))
(defun (setf session-websocket-clients) (new-value session)
  (setf (transient-session-value 'websocket-clients session) new-value))

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

(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric validate-client (chat-server client))
(defgeneric disconnect-client (chat-server client))

(defstruct client server uri host headers user-agent ws-client session account-id character-id)

(defun client-write (client string)
  (continuable
    (ws:write-to-client-text (client-ws-client client) string)))

(defun find-client (chat-server ws-client)
  (gethash ws-client (slot-value chat-server 'clients)))

(defun process-client-validation (res client json-message
                                  &aux (*acceptor* *server*)
                                  (message (jsown:parse json-message)))
  (push (cons :user-agent (jsown:val message "useragent"))
        (client-headers client))
  (let ((character-id (find-character (jsown:val message "char"))))
    (if (and (validate-client res client)
             character-id
             (eql (character-account character-id) (client-account-id client)))
        (progn
          (logit "Client validated: ~S. It's now playing as ~A."
                 client (character-name character-id))
          (setf (client-character-id client) character-id)
          (push (session-websocket-clients (client-session client)) client))
        (progn
          (logit "No session. Disconnecting client. (~S)" client)
          (disconnect-client res client)))))

(defun client-character-name (client)
  (when-let ((character-id (client-character-id client)))
    (character-name character-id)))

(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test #'eq))
   (client-main :initarg :client-main)))

(defmethod disconnect-client ((server chat-server) client)
  (let ((ws-client (client-ws-client client))
        (session (client-session client)))
    (ws:write-to-client-close ws-client)
    (when session
      (deletef (session-websocket-clients session)
               client))))

(defvar *websocket-server*)

(defun register-chat-server (client-main)
  (ws:register-global-resource
   "/chat"
   (setf *websocket-server* (make-instance 'chat-server :client-main client-main))
   (ws:origin-prefix "http://zushakon.sykosomatic.org")))

(defmethod add-client ((srv chat-server) client)
  (logit "Adding pending client ~S." client)
  (setf (gethash (client-ws-client client) (slot-value srv 'clients))
        client))

(defmethod remove-client ((srv chat-server) client)
  (remhash (client-ws-client client) (slot-value srv 'clients)))

(defmethod validate-client ((srv chat-server) client &aux (*acceptor* *server*))
  (let ((session (session-verify (make-instance 'persistent-session-request
                                                :uri (client-uri client)
                                                :remote-addr (client-host client)
                                                :headers-in (client-headers client)
                                                :acceptor *server*))))
    (when session
      (setf (client-session client) session
            (client-account-id client) (current-account session))
      client)))

(defmethod ws:resource-accept-connection ((res chat-server) resource-name headers ws-client)
  (continuable
    (let (alist-headers)
      (maphash (lambda (k v)
                 (push (cons (intern (string-upcase k) :keyword)
                             v)
                       alist-headers))
               headers)
      (add-client res (make-client :uri resource-name :headers alist-headers :ws-client ws-client))))
  t)

(defmethod ws:resource-client-disconnected ((res chat-server) ws-client
                                            &aux (client (find-client res ws-client)))
  (logit "Client ~S disconnected." client)
  (continuable (remove-client res client)))

(defmethod ws:resource-received-text ((res chat-server) ws-client message
                                      &aux (client (find-client res ws-client)))
  (continuable
    (if (client-session client)
        (process-client-message res client message)
        (process-client-validation res client message))))

(defun actor-client (actor-id)
  (maphash-values (lambda (client)
                    (when (eql actor-id (client-character-id client))
                      (return-from actor-client client)))
                  (slot-value *websocket-server* 'clients))
  nil)

(defun send-msg (actor msg)
  (client-write (actor-client actor)
                (jsown:to-json msg)))

(defun send-action (recipient actor action-txt)
  #+nil(when-let ((scene-id (session-value 'scene-id (client-session (actor-client recipient)))))
    (logit "Saving action under scene ~A: ~A" scene-id action-txt)
    (add-action scene-id actor action-txt))
  (send-msg recipient `("action" (:obj
                                  ,@(when actor
                                          `(("actor" . ,(character-name actor))))
                                  ("action" . ,action-txt)))))

(defun send-dialogue (recipient actor dialogue &optional parenthetical)
  (let ((char-name (character-name actor)))
    #+nil(when-let ((scene-id (session-value 'scene-id (client-session (actor-client recipient)))))
      (logit "Saving dialogue under scene ~A: ~A sez: (~A) ~A." scene-id char-name parenthetical dialogue)
      (add-dialogue scene-id char-name dialogue parenthetical))
    (send-msg recipient `("dialogue" (:obj
                                      ("actor" . ,char-name)
                                      ("parenthetical" . ,parenthetical)
                                      ("dialogue" . ,dialogue))))))

(defun send-transition (recipient text)
  (send-msg recipient (list "transition" text)))

(defun send-ooc (recipient actor text)
  (let ((display-name (account-display-name (client-account-id (actor-client actor)))))
    (send-msg recipient `("ooc" (:obj
                                 ("display-name" . ,display-name)
                                 ("text" . ,text))))))

(defun local-actors (actor-id)
  (declare (ignore actor-id))
  (mapcar #'client-character-id (hash-table-values (slot-value *websocket-server* 'clients))))

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

(defun process-user-input (res client input)
  (funcall (slot-value res 'client-main) (client-character-id client) input))

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
              (create-scene (client-account-id client))))))

(defun stop-recording (res client)
  (declare (ignore res))
  (logit "Request to stop recording received.")
  (delete-session-value 'scene-id (client-session client)))

;;;
;;; Init/teardown
;;;
(defvar *websocket-thread* nil)
(defvar *chat-resource-thread* nil)

(defun init-websockets (client-main &optional (port *chat-server-port*))
  (register-chat-server client-main)
  (register-session-finalizer 'websocket-clients
                              (lambda (session-id)
                                (when-let (clients (session-websocket-clients session-id))
                                  (map nil #'disconnect-client clients))))
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
    (bt:destroy-thread *websocket-thread*))
  (when (and *chat-resource-thread* (bt:thread-alive-p *chat-resource-thread*))
    (bt:destroy-thread *chat-resource-thread*))
  (setf *websocket-thread* nil
        *chat-resource-thread* nil))

;; (defun send-user-action (client user-action)
;;   (when-let ((scene-id (session-value 'scene-id (client-session client))))
;;     (save-user-action scene-id user-action))
;;   (client-write client (render-user-action-to-json user-action)))

;; (defun broadcast-user-action (res action)
;;   (maphash-values (rcurry #'send-user-action action)
;;                   (slot-value res 'clients)))
