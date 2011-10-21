(util:def-file-package #:sykosomatic.handler
  (:use :hunchentoot
        :sykosomatic.config
        :sykosomatic.template
        :sykosomatic.session)
  (:export :init-hunchentoot
           :teardown-hunchentoot
           :with-form-errors
           :ensure-logged-in
           :render-page))

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *resource-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-content-type* "text/html; charset=utf-8"
        *hunchentoot-default-external-format* :utf8)
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

(defparameter *page-template-relative-path* #p"page.html")
(defparameter *site-page-path* (merge-pathnames "site-pages/" *template-path*))

(defun render-page (body-template variables &key title)
  (render-template *page-template-relative-path*
                   (list* :page-body (list (list (merge-pathnames body-template
                                                                  *site-page-path*)))
                          :title title
                          :error-list (pop-error-list)
                          :nav-items `((:nav-href "/" :nav-title "Home")
                                       ,(if (current-account)
                                            '(:nav-href "/role" :nav-title "Choose Character")
                                            '(:nav-href "/login" :nav-title "Log In"))
                                       (:nav-href "/" :nav-title "Contact")
                                       (:nav-href "/" :nav-title "About")
                                       ,@(when (current-account)
                                               '((:nav-href "/logout" :nav-title "Log Out"))))
                          variables)))
