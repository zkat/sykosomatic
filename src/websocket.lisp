(cl:defpackage #:sykosomatic.websocket
  (:use :cl :alexandria :hunchentoot
        :sykosomatic.util
        :sykosomatic.db
        :sykosomatic.config
        :sykosomatic.session
        :sykosomatic.character
        :sykosomatic.account
        :sykosomatic.scene)
  (:export
   :generate-websocket-token
   :init-websockets
   :teardown-websockets))
(cl:in-package #:sykosomatic.websocket)

(defvar *websocket-server*)

(defun register-chat-server ()
  (ws:register-global-resource
   "/chat"
   (setf *websocket-server* (make-instance 'chat-server))
   (ws:origin-prefix "http://zushakon.sykosomatic.org")))

(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test #'eq))))

(defun session-websocket-clients (session)
  (transient-session-value 'websocket-clients session))
(defun (setf session-websocket-clients) (new-value session)
  (setf (transient-session-value 'websocket-clients session) new-value))

(defmethod ws:resource-client-disconnected ((res chat-server) ws-client
                                            &aux (client (find-client res ws-client)))
  (logit "Client ~S disconnected." client)
  (continuable (remove-client res client)))

(defmethod ws:resource-received-text ((res chat-server) ws-client message)
  (continuable
    (let ((client (find-client res ws-client)))
      (if (and client (client-session client))
          (process-client-message res client message)
          (handle-new-client res ws-client message)))))

(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric validate-client (chat-server client))
(defgeneric disconnect-client (chat-server client))

(defstruct client
  ;; Websocket metadata
  server ws-client
  ;; Metadata/validation
  host user-agent validation-token
  ;; Associated session and entity.
  session entity-id)

(defmethod add-client ((srv chat-server) client)
  (logit "Adding pending client ~S." client)
  (setf (gethash (client-ws-client client) (slot-value srv 'clients))
        client))

(defmethod remove-client ((srv chat-server) client)
  (remhash (client-ws-client client) (slot-value srv 'clients)))

(defmethod disconnect-client ((server chat-server) client)
  (let ((ws-client (client-ws-client client))
        (session (client-session client)))
    (ws:write-to-client-close ws-client)
    (when session
      (deletef (session-websocket-clients session)
               client))))

(defun client-account-id (client)
  (when-let (sess (client-session client))
    (current-account sess)))

;;;
;;; Client validation
;;;
(defparameter *validation-token-timeout* 30
  "How long before validation tokens are considered expired, in seconds.")
(defdao websocket-validation-token ()
  ((token text)
   (session-string text)
   (time-created timestamp :col-default (:now)))
  (:keys token))

(defun generate-websocket-token (session-string)
  (let ((token (generate-session-string)))
    (with-db ()
      (make-dao 'websocket-validation-token :token token :session-string session-string))
    token))

(defun find-session-string-by-token (token)
  (with-transaction ()
    (destructuring-bind (row-id session-string expiredp)
        (db-query (:for-update
                   (:select 'id 'session-string (:< (:+ 'time-created
                                                        (:raw
                                                         (format nil "interval '~A seconds'"
                                                                 *validation-token-timeout*)))
                                                    (:now))
                            :from 'websocket-validation-token
                            :where (:= 'token token)))
                  :row)
      (when row-id
        (db-query (:delete-from 'websocket-validation-token
                                :where (:or
                                        (:< (:+ 'time-created
                                                (:raw
                                                 (format nil "interval '~A seconds'"
                                                         *validation-token-timeout*)))
                                            (:now))
                                        (:= 'id row-id))))
        (and (not expiredp) session-string)))))

(defmethod validate-client ((srv chat-server) client &aux (*acceptor* *server*))
  (logit "Attempting to validate client: ~S" client)
  (when-let* ((session-string (find-session-string-by-token (client-validation-token client)))
              (session (verify-persistent-session session-string
                                                  (client-user-agent client)
                                                  (client-host client))))
    (setf (client-session client) session)
    (logit "Client successfully validated: ~S" client)
    client))

(defun handle-new-client (res ws-client json-message
                          &aux (message (jsown:parse json-message)))
  (let* ((client (make-client :ws-client ws-client
                              :user-agent (jsown:val message "useragent")
                              :validation-token (jsown:val message "token")))
         (client-valid-p (validate-client res client))
         (char-index (jsown:val message "char"))
         (character-id (nth char-index (account-characters (client-account-id client)))))
    (cond ((and client-valid-p character-id)
           (logit "Client validated: ~S.~%It's now playing as ~A."
                  client (character-name character-id))
           ;; TODO - Need to do something about clients connecting to the same entity.
           #+nil
           (when-let (existing-client (find character-id
                                            (session-websocket-clients (client-session client))
                                            :key #'client-entity-id))
             (deletef existing-client (session-websocket-clients (client-session client)))
             (logit "Something funky is cooking.")
             (disconnect-client res existing-client))
           (setf (client-entity-id client) character-id)
           (add-client res client)
           (push (session-websocket-clients (client-session client)) client))
          (t
           (logit "No session. Disconnecting client. (~S)" client)
           (disconnect-client res client)))))

(defun client-write (client string)
  (continuable
    (ws:write-to-client-text (client-ws-client client) string)))

(defun find-client (chat-server ws-client)
  (gethash ws-client (slot-value chat-server 'clients)))

(defun client-character-name (client)
  (when-let ((character-id (client-entity-id client)))
    (character-name character-id)))

(defun actor-client (actor-id)
  (maphash-values (lambda (client)
                    (when (eql actor-id (client-entity-id client))
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

(defun send-ooc (recipient display-name text)
  (send-msg recipient `("ooc" (:obj
                               ("display-name" . ,display-name)
                               ("text" . ,text)))))

(defun local-actors (actor-id)
  (declare (ignore actor-id))
  (mapcar #'client-entity-id (hash-table-values (slot-value *websocket-server* 'clients))))

;;;
;;; Client messages
;;;
(defvar *commands* (make-hash-table :test #'equal))
(defvar *client*)
(defvar *resource*)
(defvar *raw-message*)

(defun find-command (name)
  (values (gethash name *commands*)))
(defun (setf find-command) (command name)
  (setf (gethash name *commands*) command))

(defmacro defhandler (name lambda-list &body body)
  (let ((name (string-downcase (string name))))
    `(setf (find-command ,name)
           (lambda ,lambda-list
             ,@body))))

(defun process-client-message (res client raw-message)
  (logit "Client message: ~A" raw-message)
  (when-let (message (handler-case (jsown:parse raw-message)
                       (error (e) (logit "Error while parsing client message '~A': ~A" raw-message e))))
    (if-let (command (find-command (car message)))
      (let ((*client* client)
            (*resource* res)
            (*raw-message* raw-message))
        (handler-case
            (apply command (cdr message))
          (error (e)
            (logit "Got an error while the handler for '~A': ~A" raw-message e))))
      (logit "Unknown command '~A'. Ignoring." (car message)))))

(defhandler ooc (message)
  (map nil (rcurry #'send-ooc (account-display-name (client-account-id *client*)) message)
       (local-actors *client*)))

(defhandler dialogue (message)
  (handler-case
      (multiple-value-bind (dialogue parenthetical) (sykosomatic.parser:parse-dialogue message)
        (map nil (rcurry #'send-dialogue (client-entity-id *client*) dialogue parenthetical)
             (local-actors *client*)))
    (error (e)
      (send-msg (client-entity-id *client*) (list "parse-error" (princ-to-string e))))))

(defhandler action (action-txt)
  (handler-case
      (let ((predicate (sykosomatic.parser:parse-action action-txt)))
        (map nil (rcurry #'send-action (client-entity-id *client*) predicate)
             (local-actors *client*)))
    (error (e)
      (send-msg (client-entity-id *client*) (list "parse-error" (princ-to-string e))))))

(defhandler emit (text)
  (map nil (rcurry #'send-action nil text)
       (local-actors *client*)))

(defhandler complete-action (action-text)
  (client-write *client* (jsown:to-json (list "completion"
                                              (sykosomatic.parser:action-completions action-text)))))

(defhandler char-desc (charname)
  (logit "Got a character description request: ~S" charname)
  (client-write *client* (jsown:to-json (list "char-desc"
                                              (character-description (find-character charname))))))

(defhandler ping ()
  (client-write *client* (jsown:to-json (list "pong"))))

(defhandler start-recording ()
  (logit "Request to start recording received.")
  (if (session-value 'scene-id (client-session *client*))
      (logit "Scene already being recorded. Ignoring request.")
      (let ((*acceptor* *server*))
        (setf (session-value 'scene-id (client-session *client*))
              (create-scene (client-account-id *client*))))))

(defhandler stop-recording ()
  (logit "Request to stop recording received.")
  (delete-session-value 'scene-id (client-session *client*)))

;;;
;;; Init/teardown
;;;
(defvar *websocket-thread* nil)
(defvar *chat-resource-thread* nil)

(defun init-websockets (&optional (port *chat-server-port*))
  (register-chat-server)
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


;;; User actions
;; (defstruct (user-action (:constructor make-user-action (user action dialogue
;;                                                         &optional (timestamp (get-universal-time)))))
;;   user timestamp action dialogue)

;; (defun render-user-action-to-json (user-action)
;;   (let ((action (user-action-action user-action))
;;         (dialogue (user-action-dialogue user-action))
;;         (user (user-action-user user-action)))
;;     (jsown:to-json
;;      `("user-action"
;;        (:obj ("action" . ,action)
;;              ("character" . ,user)
;;              ("dialogue" . ,dialogue))))))


;; (defun send-user-action (client user-action)
;;   (when-let ((scene-id (session-value 'scene-id (client-session client))))
;;     (save-user-action scene-id user-action))
;;   (client-write client (render-user-action-to-json user-action)))

;; (defun broadcast-user-action (res action)
;;   (maphash-values (rcurry #'send-user-action action)
;;                   (slot-value res 'clients)))
