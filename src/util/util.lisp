(cl:defpackage #:sykosomatic.util
  (:use :cl :alexandria :cl-speedy-queue :optima)
  (:nicknames :util)
  (:export :def-file-package
           :logit
           :dbg
           :continuable
           :*english-list-format-string*
           :random-string
           :random-byte-array
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
           :dequeue
           ;; Pattern matching
           :cmatch
           :multiple-value-cmatch
           :lambda-cmatch))
(cl:in-package :sykosomatic.util)

;; Because SAFETY settings persist from libraries in CCL >:|
(declaim (optimize (safety 1)))

(rename-package (find-package :cl-form) :cl-form (cons :sykosomatic.util.form
                                                       (remove :sykosomatic.util.form
                                                               (package-nicknames (find-package :cl-form))
                                                               :test #'string=)))

(defmacro def-file-package (name &body defpackage-args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpackage ,name
       (:use :cl :alexandria :sykosomatic.util)
       ,@defpackage-args)
     (in-package ,name)))
(when (find-package :pxref)
  (pushnew 'def-file-package
           (symbol-value (intern "*HANDLE-PACKAGE-FORMS*" :pxref))))

(defmacro continuable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(with-simple-restart (continue "Continue") ,@body))

(defparameter *english-list-format-string*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defun logit (format-string &rest format-args)
  (format t "~&~A~%" (apply #'format nil format-string format-args)))

(defun dbg (comment obj)
  (format t "DBG - ~A (~S)" comment obj)
  obj)

(defun insecure-random-string (length &optional (dictionary "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (map-into (make-array length :element-type 'character)
            (curry #'random-elt dictionary)))

(defun random-string (length)
  "LENGTH will be rounded down to the nearest multiple of two."
  (ironclad:byte-array-to-hex-string (cl+ssl:random-bytes (truncate length 2))))

(defun random-byte-array (length)
  (cl+ssl:random-bytes length))
