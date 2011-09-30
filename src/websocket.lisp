(cl:defpackage #:sykosomatic.websocket
  (:use :cl :alexandria :hunchentoot
        :sykosomatic.util
        :sykosomatic.db
        :sykosomatic.config
        :sykosomatic.session
        :sykosomatic.game-objects.nameable
        :sykosomatic.account
        :sykosomatic.scene)
  (:export
   :generate-websocket-token
   :init-websockets
   :teardown-websockets))
(cl:in-package #:sykosomatic.websocket)

(defvar *websocket-server*)

(defun register-chat-server (server)
  (ws:register-global-resource
   (resource-name server)
   (setf *websocket-server* server)
   (ws:origin-prefix (origin-prefix server))))

(defgeneric origin-prefix (server))
(defgeneric resource-name (server))
(defgeneric add-client (chat-server client))
(defgeneric remove-client (chat-server client))
(defgeneric find-client (chat-server client))
(defgeneric validate-client (chat-server client))
(defgeneric disconnect-client (chat-server client))

(defstruct client
  ;; Websocket metadata
  server ws-client
  ;; Metadata/validation
  host user-agent validation-token
  ;; Associated session and entity.
  session entity-id)

;;;
;;; Client utils
;;;
(defun client-write (client string)
  (continuable
    (ws:write-to-client-text (client-ws-client client) string)))

(defun client-write-json (client obj)
  (client-write client (jsown:to-json obj)))

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

(defparameter *websocket-token-length* 256)
(defun generate-websocket-token (session-string)
  (let ((token (random-string *websocket-token-length*)))
    (with-db ()
      (make-dao 'websocket-validation-token :token token :session-string session-string))
    token))

(defun find-session-string-by-token (token)
  (with-transaction ()
    (when-let (session-info
               (db-query (:for-update
                          (:select 'id 'session-string (:< (:+ 'time-created
                                                               (:raw
                                                                (format nil "interval '~A seconds'"
                                                                        *validation-token-timeout*)))
                                                           (:now))
                                   :from 'websocket-validation-token
                                   :where (:= 'token token)))
                         :row))
      (destructuring-bind (row-id session-string expiredp)
          session-info
        (when row-id
          (db-query (:delete-from 'websocket-validation-token
                                  :where (:or
                                          (:< (:+ 'time-created
                                                  (:raw
                                                   (format nil "interval '~A seconds'"
                                                           *validation-token-timeout*)))
                                              (:now))
                                          (:= 'id row-id))))
          (and (not expiredp) session-string))))))

(defun handle-new-client (res ws-client json-message
                          &aux (message (jsown:parse json-message)))
  (let* ((client (make-client :ws-client ws-client
                              :user-agent (jsown:val message "useragent")
                              :validation-token (jsown:val message "token")))
         (client-valid-p (validate-client res client))
         (char-index (jsown:val message "char"))
         (entity-id (nth char-index (account-bodies
                                     (client-account-id client)))))
    (cond ((and client-valid-p entity-id)
           (logit "Client validated: ~S.~%It's now playing as ~A."
                  client (full-name entity-id))
           ;; TODO - Need to do something about clients connecting to the same entity.
           (setf (client-entity-id client) entity-id)
           (add-client res client))
          (t
           (logit "No session. Disconnecting client. (~S)" client)
           (disconnect-client res client)))))

;;;
;;; Protocol implementations
;;;
(defclass chat-server (ws:ws-resource)
  ((clients :initform (make-hash-table :test #'eq))
   (origin-prefix :initarg :origin-prefix :reader origin-prefix)
   (resource-name :initarg :resource-name :reader resource-name)))

(defmethod ws:resource-client-disconnected ((res chat-server) ws-client
                                            &aux (client (find-client res ws-client)))
  (logit "Client ~S disconnected." client)
  (continuable (remove-client res client)))

(defmethod ws:resource-received-text ((res chat-server) ws-client message)
  (continuable
    (let ((client (find-client res ws-client)))
      (if (and client (client-session client))
          (handle-client-message res client message)
          (handle-new-client res ws-client message)))))

(defmethod add-client ((srv chat-server) client)
  (logit "Adding pending client ~S." client)
  (setf (gethash (client-ws-client client) (slot-value srv 'clients))
        client))

(defmethod remove-client ((srv chat-server) client)
  (remhash (client-ws-client client) (slot-value srv 'clients)))

(defmethod find-client ((res chat-server) ws-client)
  (gethash ws-client (slot-value res 'clients)))

(defmethod disconnect-client ((server chat-server) client)
  (ws:write-to-client-close (client-ws-client client)))

(defmethod validate-client ((srv chat-server) client &aux (*acceptor* *server*))
  (logit "Attempting to validate client: ~S" client)
  (when-let* ((session-string (find-session-string-by-token (client-validation-token client)))
              (session (verify-persistent-session session-string
                                                  (client-user-agent client)
                                                  (client-host client))))
    (setf (client-session client) session)
    (logit "Client successfully validated: ~S" client)
    client))

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

(defun handle-client-message (res client raw-message)
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

;;;
;;; Messaging utilities
;;;
(defun entity-client (entity-id)
  (maphash-values (lambda (client)
                    (when (eql entity-id (client-entity-id client))
                      (return-from entity-client client)))
                  (slot-value *websocket-server* 'clients))
  nil)

(defun all-clients ()
  (hash-table-values (slot-value *websocket-server* 'clients)))

;;;
;;; Handlers
;;;
(defun send-ooc (recipient display-name text)
  (client-write-json recipient `("ooc" (:obj
                                        ("display-name" . ,display-name)
                                        ("text" . ,text)))))

(defhandler ooc (message)
  (map nil (rcurry #'send-ooc (account-display-name (client-account-id *client*)) message)
       (all-clients)))

(defun send-dialogue (recipient actor dialogue &optional parenthetical)
  (let ((char-name (full-name actor)))
    #+nil(when-let ((scene-id (session-value 'scene-id (client-session (entity-client recipient)))))
           (logit "Saving dialogue under scene ~A: ~A sez: (~A) ~A." scene-id char-name parenthetical dialogue)
           (add-dialogue scene-id char-name dialogue parenthetical))
    (client-write-json recipient `("dialogue" (:obj
                                               ("actor" . ,char-name)
                                               ("parenthetical" . ,parenthetical)
                                               ("dialogue" . ,dialogue))))))

(defhandler dialogue (message)
  (handler-case
      (multiple-value-bind (dialogue parenthetical) (sykosomatic.parser:parse-dialogue message)
        (map nil (rcurry #'send-dialogue (client-entity-id *client*) dialogue parenthetical)
             (all-clients)))
    (error (e)
      (client-write-json *client* (list "parse-error" (princ-to-string e))))))

(defun send-action (recipient actor action-txt)
  #+nil(when-let ((scene-id (session-value 'scene-id (client-session (entity-client recipient)))))
         (logit "Saving action under scene ~A: ~A" scene-id action-txt)
         (add-action scene-id actor action-txt))
  (client-write-json recipient `("action" (:obj
                                           ,@(when actor
                                                   `(("actor" . ,(full-name actor))))
                                           ("action" . ,action-txt)))))

(defhandler action (action-txt)
  (handler-case
      (let ((predicate (sykosomatic.parser:parse-action action-txt)))
        (map nil (rcurry #'send-action (client-entity-id *client*) predicate)
             (all-clients)))
    (error (e)
      (client-write-json *client* (list "parse-error" (princ-to-string e))))))

(defhandler emit (text)
  (map nil (rcurry #'send-action nil text)
       (all-clients)))

(defhandler complete-action (action-text)
  (client-write-json *client* (list "completion"
                                    (sykosomatic.parser:parse-action-completions action-text))))

(defhandler obj-desc (objname)
  (logit "Got an object description request: ~S" objname)
  (when-let (entity (find-by-full-name objname :fuzzyp t))
    (client-write-json *client* (list "obj-desc" (full-name entity)))))

(defhandler ping ()
  (client-write-json *client* (list "pong")))

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

(defun init-websockets (&key (port *websocket-server-port*)
                        (resource-name *websocket-resource-name*)
                        (origin-prefix *server-base-url*))
  (register-chat-server (make-instance 'chat-server
                                       :origin-prefix origin-prefix
                                       :resource-name resource-name))
  (setf *websocket-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (ws:run-server port))
         :name "websockets server"))
  (setf *chat-resource-thread*
        (bt:make-thread
         (lambda () (ws:run-resource-listener (ws:find-global-resource resource-name)))
         :name "chat resource listener")))

(defun teardown-websockets ()
  (when (and *websocket-thread* (bt:thread-alive-p *websocket-thread*))
    (bt:destroy-thread *websocket-thread*))
  (when (and *chat-resource-thread* (bt:thread-alive-p *chat-resource-thread*))
    (bt:destroy-thread *chat-resource-thread*))
  (setf *websocket-thread* nil
        *chat-resource-thread* nil))
