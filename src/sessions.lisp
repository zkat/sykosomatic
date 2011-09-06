(cl:in-package :sykosomatic)

(optimizations)

(defclass persistent-session-request (hunchentoot:request)
  ())

(defdao persistent-session ()
  ((id :col-type serial :reader sykosomatic.db:id)
   (cookie-value :col-type text :initform (random-string 32) :reader session-cookie-value)
   (account-id :col-type bigint :initarg :account-id)
   (user-agent :col-type text :initform (user-agent *request*))
   (remote-addr :col-type text :initform (real-remote-addr *request*))
   (session-start :col-type timestamp :col-default (:now))
   (last-seen :col-type timestamp :col-default (:now))
   (max-time :col-type interval :initarg :max-time :initform (format nil "~A seconds"
                                                                     *session-max-time*)))
  (:keys cookie-value)
  (:unique cookie-value))

(defun current-account (&optional (session *session*))
  (when session
    (with-db ()
      (query (:select 'account-id :from 'persistent-session
                      :where (:= 'id session))
             :single))))

(defun ensure-logged-in ()
  (unless *session*
    (push-error "You must be logged in to access that page.")
    (redirect "/login")))

(defun persistent-session-gc ()
  (with-db ()
    (let ((old-session-ids (query (:select 'id :from 'persistent-session
                                           :where (:< (:+ 'last-seen 'max-time)
                                                      (:now)))
                                  :column)))
      (map nil #'session-cleanup old-session-ids))))

(defun start-persistent-session (account-id)
  (or *session*
      (let ((session (with-db () (make-dao 'persistent-session :account-id account-id))))
        (set-cookie (session-cookie-name *acceptor*)
                    :value (session-cookie-value session)
                    :path "/")
        #+nil(persistent-session-gc)
        (setf *session* (id session)))))

(defmethod session-verify ((request persistent-session-request))
  (let ((session-identifier (or (cookie-in (session-cookie-name *acceptor*) request)
                                (get-parameter (session-cookie-name *acceptor*) request))))
    (when (and session-identifier
               (stringp session-identifier)
               (not (emptyp session-identifier)))
      (with-db ()
        (with-transaction ()
          (when-let (session-id (query (:for-update
                                        (:select 'id :from 'persistent-session
                                                 :where (:and (:= 'cookie-value session-identifier)
                                                              (:= 'user-agent (user-agent request))
                                                              (:= 'remote-addr (remote-addr request)))))
                                       :single))
            (if (query (:for-update
                        (:select t :from 'persistent-session
                                 :where (:and (:= 'id session-id)
                                              (:< (:+ 'last-seen 'max-time)
                                                  (:now))))))
                (logout session-id)
                (prog1 session-id
                  (query (:update 'persistent-session
                                  :set 'last-seen (:now)
                                  :where (:= 'id session-id)))))))))))

(defun logout (session-id)
  (let ((account-id (current-account session-id))
        (websocket-clients (session-websocket-clients session-id)))
    (when account-id
      (logit "~A logged out." (account-email account-id)))
    (when websocket-clients
      (mapc #'disconnect-client websocket-clients))
    (with-db ()
      (query (:delete-from 'persistent-session :where (:= 'id session-id))))))

(defun session-cleanup (session-id)
  (logit "Session timed out. Trying to log it out...")
  (logout session-id))

(defvar *transient-session-values* (make-hash-table))

(defun remove-transient-session-values (&optional (session *session*))
  (remhash session *transient-session-values*))

(defun transient-session-value (key &optional (session *session*))
  (when-let (session-table (gethash session *transient-session-values*))
    (gethash key session-table)))
(defun (setf transient-session-value) (new-value key &optional (session *session*))
  (let ((session-table (or (gethash session *transient-session-values*)
                           (setf (gethash session *transient-session-values*)
                                 (make-hash-table)))))
    (setf (gethash key session-table) new-value)))

(defun session-websocket-clients (session)
  (transient-session-value 'websocket-clients session))
(defun (setf session-websocket-clients) (new-value session)
  (setf (transient-session-value 'websocket-clients session) new-value))

(defun push-error (format-string &rest format-args)
  (push (apply #'format nil format-string format-args)
        (transient-session-value 'errors)))

(defun session-errors (&optional (session hunchentoot:*session*))
  (transient-session-value 'errors session))
(defun (setf session-errors) (new-value &optional (session hunchentoot:*session*))
  (setf (transient-session-value 'errors session) new-value))
