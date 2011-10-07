(util:def-file-package #:sykosomatic.handler
  (:use :hunchentoot
        :sykosomatic.config
        :sykosomatic.session)
  (:export :init-hunchentoot
           :teardown-hunchentoot
           :with-form-errors
           :ensure-logged-in
           :pop-error-list))

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *resource-path*)
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

(defun ensure-logged-in ()
  (unless *session*
    (push-error "You must be logged in to access that page.")
    (redirect "/login")))

(defun pop-error-list ()
  (loop for error in (session-errors)
     collect (list :error error)
     finally (setf (session-errors) nil)))
