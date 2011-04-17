(cl:defpackage #:sykosomatic
  (:use #:cl #:alexandria #:hunchentoot #:yaclml #:sykosomatic.account #:sykosomatic.character
        #:sykosomatic.scene)
  (:import-from #:sykosomatic.db #:init-db))
(cl:in-package #:sykosomatic)

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

(defun render-user-action (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action)))
    (<:div :class "user-entry"
      (if (and (not (emptyp action)) (emptyp dialogue))
          (<:p :class "action"
               (<:ah (user-action-user user-action) " " action))
          (progn
            (<:p :class "character"
                 (<:ah (user-action-user user-action)))
            (unless (emptyp action)
              (<:p :class "parenthetical"
                   (<:ah "(" action ")")))
            (<:p :class "dialogue"
                 (<:ah
                  (if (emptyp dialogue)
                      "..."
                      dialogue))))))))
