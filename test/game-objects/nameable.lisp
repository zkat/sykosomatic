(cl:defpackage #:sykosomatic.test.game-objects.nameable
  (:use :cl :alexandria :eos
        :sykosomatic.test
        :sykosomatic.entity
        :sykosomatic.db
        :sykosomatic.game-objects.nameable))
(cl:in-package #:sykosomatic.test.game-objects.nameable)

(def-suite game-objects.nameable :in sykosomatic)
(in-suite game-objects.nameable)

(defun gen-full-name (full-name-arg-alist)
  (let ((e (create-entity)))
    (unwind-protect
         (progn
           (apply #'add-name e
                  (cdr (assoc 'base-name full-name-arg-alist))
                  (alist-plist
                   (remove :base-name
                           (mapcar (lambda (pair)
                                     (cons (intern (string (car pair)) :keyword)
                                           (cdr pair)))
                                   full-name-arg-alist)
                           :key #'car)))
           
           (full-name e))
      (db-query (:delete-from 'nameable :where (:= 'entity-id e)))
      (db-query (:delete-from 'entity :where (:= 'id e))))))

(test full-name
  (macrolet ((test-case (expected arg-alist)
               `(is (string= ,expected (gen-full-name ,arg-alist)))))
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
                                                   (suffix-titles . #("PhD" "Esq"))))))
