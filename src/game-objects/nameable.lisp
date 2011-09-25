(cl:defpackage #:sykosomatic.game-objects.nameable
  (:use :cl :alexandria :postmodern :sykosomatic.entity :sykosomatic.db :sykosomatic.util)
  (:export :base-name :full-name))
(cl:in-package #:sykosomatic.game-objects.nameable)

(defdao nameable ()
  ((id :col-type serial :reader id)
   (entity-id :col-type bigint :initarg :entity-id)
   (base-name :col-type text :initarg :base-name)
   (use-article-p :col-type boolean :col-default nil :initarg :use-article-p)
   (adjectives :col-type (or db-null text[]) :initarg :adjectives)
   (titles :col-type (or db-null text[]) :initarg :titles)
   (first-name :col-type (or db-null text) :initarg :first-name)
   (suffix :col-type (or db-null text) :initarg :suffix)
   (suffix-titles :col-type (or db-null text[]) :initarg :suffix-titles)))

(defun base-name (entity)
  (with-db ()
    (query (:select 'base-name :from 'nameable :where (:= 'entity-id entity))
           :single)))

;; Thank you, PCL
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defun full-name (entity)
  (macrolet ((unless-null (maybe-null &body body)
               `(unless (eq :null ,maybe-null)
                  ,@body)))
    (with-db ()
      (when-let (row (query (:select :* :from 'nameable :where (:= 'entity-id entity))
                            :row))
        (destructuring-bind (id entity-id base-name use-article-p adjectives
                                titles first-name suffix suffix-titles)
            row
            (declare (ignore id entity-id))
          (with-output-to-string (s)
            (when use-article-p (princ "a " s))
            (unless-null adjectives (format s *english-list* (coerce adjectives 'list)) (princ " " s))
            (unless-null titles (map nil (curry #'format s "~A ") titles))
            (unless-null first-name (format s "~A " first-name))
            (princ base-name s)
            (unless-null suffix (format s " ~A" suffix))
            (unless-null suffix-titles (format s "~{, ~A~}" (coerce suffix-titles 'list)))))))))

(defun test-names ()
  (with-db ()
    (flet ((test-case (expected nameable-alist)
             (let ((e (create-entity :comment expected)))
               (unwind-protect
                    (progn
                      (apply #'make-dao 'nameable :entity-id e
                             (alist-plist
                              (mapcar (lambda (pair)
                                        (cons (intern (string (car pair)) :keyword)
                                              (cdr pair)))
                                      nameable-alist)))
                      (assert (string= (full-name e) expected) ()
                              "Full name was ~S" (full-name e)))
                 (with-db ()
                   (query (:delete-from 'nameable :where (:= 'entity-id e)))
                   (query (:delete-from 'entity :where (:= 'id e))))))))
      (test-case "a little teapot"
                 '((base-name . "teapot")
                   (use-article-p . t)
                   (adjectives . #("little"))))
      (test-case "a short and stout teapot"
                 '((base-name . "teapot")
                   (use-article-p . t)
                   (adjectives . #("short" "stout"))))
      (test-case "a little, short, and stout teapot"
                 '((base-name . "teapot")
                   (use-article-p . t)
                   (adjectives . #("little" "short" "stout"))))
      (test-case "a male servant" '((base-name . "servant")
                                    (use-article-p . t)
                                    (adjectives . #("male"))))
      (test-case "Godot" '((base-name . "Godot")))
      (test-case "John Doe" '((base-name . "Doe")
                              (first-name . "John")))
      (test-case "Count Chocula" '((base-name . "Chocula")
                                   (titles . #("Count"))))
      (test-case "Supreme Commander John Doe" '((base-name . "Doe")
                                                (first-name . "John")
                                                (titles . #("Supreme" "Commander"))))
      (test-case "Commander John Doe Jr" '((base-name . "Doe")
                                           (first-name . "John")
                                           (titles . #("Commander"))
                                           (suffix . "Jr")))
      (test-case "Commander John Doe, PhD" '((base-name . "Doe")
                                             (first-name . "John")
                                             (titles . #("Commander"))
                                             (suffix-titles . #("PhD"))))
      (test-case "Commander John Doe Jr, PhD, Esq" '((base-name . "Doe")
                                                     (first-name . "John")
                                                     (titles . #("Commander"))
                                                     (suffix . "Jr")
                                                     (suffix-titles . #("PhD" "Esq"))))
      ;; TODO, maybe
      #+nil(test-case "the captain of the guard" '()))))
