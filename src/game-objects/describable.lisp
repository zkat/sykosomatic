(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria :sykosomatic.entity :sykosomatic.db :sykosomatic.util :postmodern))
(cl:in-package #:sykosomatic.game-objects.describable)

(defun base-name (entity)
  (modifier-value entity 'base-name))

;; Thank you, PCL
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defun full-name (entity)
  (with-db ()
    (when-let (base-name (base-name entity))
      (with-output-to-string (s)
        (when (modifier-value entity 'use-article-p)
          (princ "a " s))
        (when-let (adjectives (modifier-value entity 'adjectives))
          (format s *english-list* (coerce adjectives 'list))
          (princ " " s))
        (map nil (curry #'format s "~A ") (modifier-value entity 'titles))
        (when-let (first-name (modifier-value entity 'first-name))
          (format s "~A " first-name))
        (princ base-name s)
        (when-let (suffix (modifier-value entity 'suffix))
          (format s " ~A" suffix))
        (when-let (suffix-titles (modifier-value entity 'suffix-titles))
          (format s "~{, ~A~}" (coerce suffix-titles 'list)))))))

(defun short-description (entity)
  (modifier-value entity 'short-description))

(defun long-description (entity)
  (modifier-value entity 'long-description))

(defun test-names ()
  (flet ((test-case (expected modifier-alist)
           (unwind-protect
                (let ((e (create-entity :comment expected)))
                  (unwind-protect
                       (loop for (name . value) in modifier-alist
                          do (add-modifier e name value)
                          finally (assert (string= (full-name e) expected) ()
                                          "Full name was ~S" (full-name e)))
                    (with-db ()
                      (query (:delete-from 'modifier :where (:= 'entity-id e)))
                      (query (:delete-from 'entity :where (:= 'id e)))))))))
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
    #+nil(test-case "the captain of the guard" '())))
