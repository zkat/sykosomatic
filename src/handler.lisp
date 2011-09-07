(cl:defpackage #:sykosomatic.handler
  (:use :cl :sykosomatic.config :hunchentoot :sykosomatic.util :sykosomatic.session)
  (:export :init-hunchentoot :teardown-hunchentoot :with-form-errors))
(cl:in-package #:sykosomatic.handler)

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-handler* '404-handler)
  (pushnew 404 *approved-return-codes*)
  (setf *rewrite-for-session-urls* nil)
  (start (setf *server* (make-instance 'sykosomatic-acceptor
                                       :port *web-server-port*
                                       :request-class 'persistent-session-request)))
  (setf *catch-errors-p* nil))

(defun teardown-hunchentoot ()
  (when *server* (stop *server*) (setf *server* nil)))

(defmacro with-form-errors (&body body)
  `(let ((templ:*errors* (session-errors)))
     (unwind-protect (progn ,@body)
       (setf (session-errors) nil))))
