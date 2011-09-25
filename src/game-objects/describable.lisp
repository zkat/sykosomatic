(cl:defpackage #:sykosomatic.game-objects.describable
  (:use :cl :alexandria :sykosomatic.entity :sykosomatic.db :sykosomatic.util :postmodern))
(cl:in-package #:sykosomatic.game-objects.describable)

(defun base-name (entity)
  (modifier-value entity 'base-name))

(defun full-name (entity)
  (with-db ()
    (when-let (base-name (base-name entity))
      (let ((articlep (modifier-value entity 'use-article-p))
            (adjectives (modifier-value entity 'adjectives))
            (titles (modifier-value entity 'titles))
            (first-name (modifier-value entity 'first-name)))
        (with-output-to-string (s)
          (when articlep (princ "a " s))
          (loop
             with length = (length adjectives)
             for adj across adjectives
             for i below length
             do (cond ((= length 1)
                       (format s "~A " adj))
                      ((= i 0)
                       (princ adj s))
                      ((= (1+ i) length)
                       (format s ", and ~A " adj))
                      (t
                       (format s ", ~A" adj))))
          (map nil (curry #'format s "~A ") titles)
          (when first-name (format s "~A " first-name))
          (princ base-name s))))))

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
    ;; TODO, maybe
    #+nil(test-case "the captain of the guard" '())))
