(in-package :sykosomatic)

(defvar *runningp* nil)
(defun begin-shared-hallucination ()
  ;; Cleanup
  (when *runningp* (end-shared-hallucination) (warn "Restarting server."))
  ;; Database
  (init-db)
  ;; Websockets
  (init-websockets)
  ;; Hunchentoot
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-handler* '404-handler)
  (pushnew 404 *approved-return-codes*)
  (setf *session-removal-hook* 'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  (setf *catch-errors-p* nil)
  t)

(defun end-shared-hallucination ()
  (when *runningp* (stop *server*) (setf *server* nil *runningp* nil))
  (teardown-websockets))
