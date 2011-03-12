(cl:in-package #:belletrist)

(defvar *acceptor* nil)

(defun begin-shared-hallucination ()
  (hunchentoot:start (setf *acceptor* (make-instance 'hunchentoot:acceptor :port 8888))))

(defun end-shared-hallucination ()
  (when *acceptor* (hunchentoot:stop *acceptor*) (setf *acceptor* nil)))