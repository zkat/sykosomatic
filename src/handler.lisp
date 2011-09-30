(util:def-file-package #:sykosomatic.handler
  (:use :hunchentoot
        :sykosomatic.config
        :sykosomatic.session)
  (:export :init-hunchentoot :teardown-hunchentoot :with-form-errors))

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  ;; (setf *default-handler* 'sykosomatic.handler.404:404-handler)
  ;; (pushnew 404 *approved-return-codes*)
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
