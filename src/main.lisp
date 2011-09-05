(in-package :sykosomatic)

(optimizations)

(defvar *runningp* nil)
(defun begin-shared-hallucination ()
  (when *runningp* (end-shared-hallucination) (warn "Restarting server."))
  (init-entity-system)
  (init-websockets 'sykosomatic.parser:parse-input)
  (init-hunchentoot)
  (setf *runningp* t))

(defun end-shared-hallucination ()
  (teardown-websockets)
  (teardown-hunchentoot)
  (teardown-entity-system)
  (setf *runningp* nil)
  t)
