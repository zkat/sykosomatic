(cl:defpackage #:sykosomatic
  (:use #:cl #:alexandria #:hunchentoot #:yaclml #:string-case
        #:postmodern
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
