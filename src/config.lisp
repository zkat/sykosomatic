(util:def-file-package #:sykosomatic.config
  (:export :*server*
           :*server-base-url*
           :*web-server-port*
           :*websocket-server-port*
           :*websocket-resource-name*
           :*sykosomatic-path*
           :*resource-path*
           :*template-path*))

(defvar *server* nil)
(defparameter *server-base-url* "http://zushakon.sykosomatic.org")
(defparameter *web-server-port* 8888)
(defparameter *websocket-server-port*
  ;; 843 ; makes flash load faster, but can only do this as root.
  8889)
(defparameter *websocket-resource-name* "/chat")

(defparameter *sykosomatic-path* (asdf:system-source-directory 'sykosomatic))
(defparameter *resource-path* (merge-pathnames #p"res/" *sykosomatic-path*))
(defparameter *template-path* (merge-pathnames #p"template/" *sykosomatic-path*))

;; Sessions last for 30 days.
(setf hunchentoot:*session-max-time* (* 60 60 24 30))
