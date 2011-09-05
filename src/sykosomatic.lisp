(cl:defpackage #:sykosomatic
  (:use #:cl #:alexandria #:hunchentoot #:yaclml #:string-case
        #:sykosomatic.db #:sykosomatic.entity
        #:sykosomatic.account #:sykosomatic.character
        #:sykosomatic.scene #:sykosomatic.utils)
  (:import-from #:sykosomatic.db #:init-db))
(cl:in-package #:sykosomatic)

(optimizations)

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port*
  ;; 843 ; makes flash load faster, but can only do this as root.
  8889)
(defparameter *sykosomatic-path* (asdf:system-relative-pathname 'sykosomatic "res/"))

(defun session-websocket-clients (session)
  (session-value 'websocket-clients session))
(defun (setf session-websocket-clients) (new-value session)
  (setf (session-value 'websocket-clients session) new-value))

(defun current-account (&optional (*session* *session*))
  (session-value 'account-id))
(defun (setf current-account) (new-value &optional (*session* *session*))
  (setf (session-value 'account-id) new-value))
