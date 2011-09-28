(cl:defpackage #:sykosomatic.config
  (:use #:cl)
  (:export :*server*
           :*web-server-port*
           :*chat-server-port*
           :*sykosomatic-path*))
(cl:in-package #:sykosomatic.config)

(defvar *server* nil)
(defparameter *web-server-port* 8888)
(defparameter *chat-server-port*
  ;; 843 ; makes flash load faster, but can only do this as root.
  8889)
(defparameter *sykosomatic-path* (asdf:system-relative-pathname 'sykosomatic "res/"))

;; Sessions last for 30 days.
(setf hunchentoot:*session-max-time* (* 60 60 24 30))
