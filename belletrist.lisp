(cl:in-package #:belletrist)

(defvar *server* nil)

(defun begin-shared-hallucination ()
  (start (setf *server* (make-instance 'acceptor :port 8888))))

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))

(define-easy-handler (home :uri "/") (what-user-said start-over)
  (unless *session*
    (start-session))
  (when start-over (setf (session-value 'what-user-said) nil))
  (when what-user-said (push what-user-said (session-value 'what-user-said)))
  (with-yaclml-output-to-string
    (<:html
     (<:body
      (<:p (<:ah "You are in a dark, fluffy dungeon. " (or (reverse (session-value 'what-user-said))
                                                           "That is all.")))
      (<:form :name "user-story" :action "/" :method "get"
       (<:ah "What do?")
       (<:input :type "text" :name "what-user-said"))
      (<:br)
      (<:form :name "start-over" :action "/" :method "get"
       (<:input :type "submit" :name "start-over" :value "Start over"))))))
