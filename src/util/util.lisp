(cl:defpackage #:sykosomatic.util
  (:use :cl :alexandria)
  (:export :logit :dbg :continuable :random-string
           ;; Timer
           :make-timer :timer-tick
           ;; Queue
           :make-queue
           :queue-count
           :queue-length
           :queue-peek
           :queue-full-p
           :queue-empty-p
           :enqueue
           :dequeue))
(cl:in-package :sykosomatic.util)

;; Because SAFETY settings persist from libraries in CCL >:|
(declaim (optimize (safety 1)))

(defmacro continuable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(with-simple-restart (continue "Continue") ,@body))

(defun logit (format-string &rest format-args)
  (format t "~&~A~%" (apply #'format nil format-string format-args)))

(defun dbg (comment obj)
  (format t "DBG - ~A (~S)" comment obj)
  obj)

(defun pomo::\!unique (&rest target-fields &aux (target-fields (mapcar #'pomo::to-sql-name target-fields)))
  (format nil "ALTER TABLE ~A ADD CONSTRAINT ~A UNIQUE (~{~A~^, ~})"
          (pomo::to-sql-name pomo::*table-name*)
          (pomo::to-sql-name (format nil "~A_~{~A~^_~}_unique" pomo::*table-name* target-fields))
          target-fields))
(export 'pomo::\!unique (find-package :postmodern))

(defun random-string (length &optional (dictionary "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (map-into (make-array length :element-type 'character)
            (curry #'random-elt dictionary)))

(cl:in-package :s-sql)

(defun for-update/share (share-or-update form &rest args)
  (let* ((of-position (position :of args))
         (no-wait-position (position :nowait args))
         (of-tables (when of-position (subseq args (1+ of-position) no-wait-position))))
    `("(" ,@(sql-expand form) ,(format nil " FOR ~:@(~A~)" share-or-update)
          ,@(when of-tables (list (format nil " OF ~{~A~^, ~}" (mapcar #'sql-compile of-tables))))
          ,@(when no-wait-position (list " NOWAIT"))
          ")")))

(def-sql-op :for-update (form &rest args)
  (apply #'for-update/share "UPDATE" form args))

(def-sql-op :for-share (form &rest args)
  (apply #'for-update/share "SHARE" form args))
