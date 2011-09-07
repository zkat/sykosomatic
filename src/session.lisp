(cl:defpackage #:sykosomatic.session
  (:use :cl :hunchentoot :alexandria :postmodern
        :sykosomatic.db
        :sykosomatic.config
        :sykosomatic.account
        :sykosomatic.util)
  (:export :sykosomatic-acceptor :persistent-session-request :persistent-session
           :current-account :ensure-logged-in :start-persistent-session
           :persistent-session-gc :logout :session-cleanup
           :push-error :session-errors :session-websocket-clients))
(cl:in-package :sykosomatic.session)

(optimizations)

(defclass sykosomatic-acceptor (acceptor)
  ())

(defclass persistent-session-request (request)
  ())

(defdao persistent-session ()
  ((id :col-type serial :reader sykosomatic.db:id)
   (cookie-value :col-type text :initform (random-string 256) :reader session-cookie-value)
   (account-id :col-type bigint :initarg :account-id)
   (user-agent :col-type text :initform (user-agent *request*))
   (last-remote-addr :col-type text :initform (real-remote-addr *request*))
   (session-start :col-type timestamp :col-default (:now))
   (last-seen :col-type timestamp :col-default (:now))
   (max-time :col-type interval :initarg :max-time
             :initform (format nil "~A seconds"
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

(defmethod session-cookie-name ((acceptor sykosomatic-acceptor))
  "sykosomatic-session")

(defun persistent-session-gc ()
  (with-db ()
    (let ((old-session-ids (query (:select 'id :from 'persistent-session
                                           :where (:< (:+ 'last-seen 'max-time)
                                                      (:now)))
                                  :column)))
      (map nil #'session-cleanup old-session-ids))))

(defun start-persistent-session (account-id)
  (or (when (eql account-id (current-account *session*)) *session*)
      (let ((session (with-db () (make-dao 'persistent-session :account-id account-id))))
        (set-cookie (session-cookie-name *acceptor*)
                    :value (session-cookie-value session)
                    :path "/"
                    :secure *ssl-enabled-p*
                    :http-only t)
        (setf (session *request*) session)
        #+nil(persistent-session-gc)
        (setf *session* (id session)))))

(defmethod session-cookie-value ((session-id integer))
  (with-db ()
    (query (:select 'cookie-value :from 'persistent-session
                    :where (:= 'id session-id))
           :single)))

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
                                                              (:= 'user-agent (user-agent request)))))
                                       :single))
            (if (query (:for-update
                        (:select t :from 'persistent-session
                                 :where (:and (:= 'id session-id)
                                              (:< (:+ 'last-seen 'max-time)
                                                  (:now)))))
                       :single)
                (session-cleanup session-id)
                (prog1 session-id
                  (query (:update 'persistent-session
                                  :set 'last-seen (:now)
                                       'last-remote-addr (remote-addr request)
                                  :where (:= 'id session-id)))))))))))

(defun logout (session-id)
  (let ((account-id (current-account session-id))
        #+nil(websocket-clients (session-websocket-clients session-id)))
    (when account-id
      (logit "~A logged out." (account-email account-id)))
    #+nil(when websocket-clients
      (mapc #'sykosomatic.websocket::disconnect-client websocket-clients))
    (with-db ()
      (query (:delete-from 'persistent-session :where (:= 'id session-id))))))

(defun session-cleanup (session-id)
  (logit "Session timed out. Logging it out.")
  (logout session-id))

;;; Transient session values

;; TODO - put a lock on these.
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
