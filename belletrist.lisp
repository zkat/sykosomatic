(cl:in-package #:belletrist)

(defvar *server* nil)

(defun begin-shared-hallucination ()
  (start (setf *server* (make-instance 'acceptor :port 8888))))

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))

(define-easy-handler (home :uri "/") ()
  (setf (content-type*) "text/plain")
  (format nil "You are in a dark, stinky dungeon. That is all."))
