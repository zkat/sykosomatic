(cl:defpackage #:sykosomatic
  (:use :cl
        :sykosomatic.util
        :sykosomatic.entity
        :sykosomatic.websocket
        :sykosomatic.handler)
  (:export :begin-shared-hallucination
           :end-shared-hallucination))
(cl:in-package :sykosomatic)

(defvar *runningp* nil)
(defun begin-shared-hallucination ()
  (when *runningp* (end-shared-hallucination) (warn "Restarting server."))
  (init-entity-system)
  (init-websockets)
  (init-hunchentoot)
  (setf *runningp* t))

(defun end-shared-hallucination ()
  (teardown-websockets)
  (teardown-hunchentoot)
  (teardown-entity-system)
  (setf *runningp* nil)
  t)
