(cl:defpackage #:sykosomatic.utils
  (:use :cl)
  (:export :logit :dbg :continuable))
(cl:in-package :sykosomatic.utils)

(defmacro continuable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(with-simple-restart (continue "Continue") ,@body))

(defun logit (format-string &rest format-args)
  (format t "~&~A~%" (apply #'format nil format-string format-args)))

(defun dbg (comment obj)
  (format t "DBG XXX: ~A (~S)" comment obj)
  obj)

(defun pomo::\!unique (&rest target-fields &aux (target-fields (mapcar #'pomo::to-sql-name target-fields)))
  (format nil "ALTER TABLE ~A ADD CONSTRAINT ~A UNIQUE (~{~A~^, ~})"
          (pomo::to-sql-name pomo::*table-name*)
          (pomo::to-sql-name (format nil "~A_~{~A~^_~}_unique" pomo::*table-name* target-fields))
          target-fields))
(export 'pomo::\!unique (find-package :postmodern))
