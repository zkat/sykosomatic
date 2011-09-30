(util:def-file-package #:sykosomatic.session
  (:use :hunchentoot
        :sykosomatic.db
        :sykosomatic.config
        :sykosomatic.account)
  (:export :sykosomatic-acceptor :persistent-session-request :persistent-session
           :current-account :ensure-logged-in :start-persistent-session
           :generate-session-string :session-string
           :verify-persistent-session :persistent-session-gc :end-session :session-cleanup
           :register-session-finalizer :unregister-session-finalizer
           :transient-session-value
           :push-error :session-errors :session-websocket-clients))

(defclass sykosomatic-acceptor (acceptor)
  ())

(defclass persistent-session-request (request)
  ())

(defparameter *session-string-length* 256)

(defun generate-session-string (&optional (length *session-string-length*))
  (random-string length))

(defdao persistent-session ()
  ((cookie-value text :initform (generate-session-string) :reader session-cookie-value)
   (account-id bigint)
   (user-agent text :initform (user-agent *request*))
   (last-remote-addr text :initform (real-remote-addr *request*))
   (session-start timestamp :col-default (:now))
   (last-seen timestamp :col-default (:now))
   (max-time interval :initform (format nil "~A seconds" *session-max-time*)))
  (:keys cookie-value)
  (:unique cookie-value))

(defun current-account (&optional (session *session*))
  (when session
    (db-query (:select 'account-id :from 'persistent-session
                       :where (:= 'id session))
              :single)))

(defun session-string (&optional (session *session*))
  (db-query (:select 'cookie-value :from 'persistent-session
                     :where (:= 'id session))
            :single))

(defun ensure-logged-in ()
  (unless *session*
    (push-error "You must be logged in to access that page.")
    (redirect "/login")))

(defmethod session-cookie-name ((acceptor sykosomatic-acceptor))
  "sykosomatic-session")

(defun persistent-session-gc ()
  (with-transaction ()
    (doquery (:for-update
              (:select 'id :from 'persistent-session
                       :where (:< (:+ 'last-seen 'max-time)
                                  (:now))))
        (old-id)
      (with-db (:reusep nil) (session-cleanup old-id)))))

(defun start-persistent-session (account-id)
  (or (when (eql account-id (current-account *session*)) *session*)
      (let ((session (with-db () (make-dao 'persistent-session :account-id account-id))))
        (set-cookie (session-cookie-name *acceptor*)
                    :value (session-cookie-value session)
                    :path "/"
                    :secure (ssl-p)
                    :http-only t)
        (setf (session *request*) session)
        #+nil(persistent-session-gc)
        (setf *session* (id session)))))

(defmethod session-cookie-value ((session-id integer))
  (db-query (:select 'cookie-value :from 'persistent-session
                     :where (:= 'id session-id))
            :single))

(defun verify-persistent-session (session-identifier user-agent remote-addr)
  (with-transaction ()
    (when-let (session-info (db-query (:for-update
                                       (:select 'id (:< (:+ 'last-seen 'max-time)
                                                        (:now))
                                                :from 'persistent-session
                                                :where (:and (:= 'cookie-value session-identifier)
                                                             (:= 'user-agent user-agent))))
                                      :row))
      (destructuring-bind (session-id expiredp)
          session-info
        (when session-id
          (if expiredp
              (session-cleanup session-id)
              (db-query (:update 'persistent-session
                                 :set 'last-seen (:now)
                                 'last-remote-addr remote-addr
                                 :where (:= 'id session-id)
                                 :returning 'id)
                        :single)))))))

(defmethod session-verify ((request persistent-session-request))
  (let ((session-identifier (or (cookie-in (session-cookie-name *acceptor*) request)
                                (get-parameter (session-cookie-name *acceptor*) request))))
    (when (and session-identifier
               (stringp session-identifier)
               (not (emptyp session-identifier)))
      (verify-persistent-session session-identifier (user-agent request) (remote-addr request)))))

(defun end-session (session-id)
  (with-transaction ()
    (let ((account-id (current-account session-id)))
      (when account-id
        (logit "~A logged out." (account-email account-id)))
      (map nil (rcurry #'funcall session-id) (all-finalizers))
      (db-query (:delete-from 'persistent-session :where (:= 'id session-id))))))

(defun session-cleanup (session-id)
  (logit "Session timed out. Logging it out.")
  (end-session session-id))

;;; Session finalizers
;;; - When end-session is called, it will execute all registered finalizers, in no particular order,
;;;   by calling them on the session-id. These finalizers will all be called before the persistent
;;;   session is deleted.

(defvar *finalizer-lock* (bt:make-lock))
(defvar *session-finalizers* (make-hash-table))

(defun register-session-finalizer (name function)
  (bt:with-lock-held (*finalizer-lock*)
    (setf (gethash name *session-finalizers*) function)))
(defun unregister-session-finalizer (name)
  (bt:with-lock-held (*finalizer-lock*)
    (remhash name *session-finalizers*)))
(defun find-finalizer (name)
  (bt:with-lock-held (*finalizer-lock*)
    (gethash name *session-finalizers*)))
(defun all-finalizers ()
  (bt:with-lock-held (*finalizer-lock*)
    (hash-table-values *session-finalizers*)))

;;; Transient session values
(defvar *transient-value-lock* (bt:make-lock))
(defvar *transient-session-values* (make-hash-table))

(defun remove-transient-session-values (&optional (session *session*))
  (bt:with-lock-held (*transient-value-lock*)
    (remhash session *transient-session-values*)))

(defun transient-session-value (key &optional (session *session*))
  (bt:with-lock-held (*transient-value-lock*)
    (when-let (session-table (gethash session *transient-session-values*))
      (gethash key session-table))))
(defun (setf transient-session-value) (new-value key &optional (session *session*))
  (bt:with-lock-held (*transient-value-lock*)
    (let ((session-table (or (gethash session *transient-session-values*)
                             (setf (gethash session *transient-session-values*)
                                   (make-hash-table)))))
      (setf (gethash key session-table) new-value))))

(defun push-error (format-string &rest format-args)
  (bt:with-lock-held (*transient-value-lock*)
    (push (apply #'format nil format-string format-args)
          (transient-session-value 'errors))))

(defun session-errors (&optional (session hunchentoot:*session*))
  (transient-session-value 'errors session))
(defun (setf session-errors) (new-value &optional (session hunchentoot:*session*))
  (setf (transient-session-value 'errors session) new-value))
