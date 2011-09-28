(cl:defpackage #:sykosomatic.vocabulary
  (:use :cl :alexandria :postmodern :sykosomatic.db)
  (:export :add-pronoun :remove-pronoun :add-adverb :remove-adverb
           :add-verb :remove-verb :verb-completions
           :third-person-singular :preterite))
(cl:in-package #:sykosomatic.vocabulary)

;;; Pronouns
(defdao pronoun ()
  ((id :col-type serial :reader id)
   (pluralp :col-type boolean :col-default nil :initarg :pluralp)
   ;; "She"/"He"
   (subjective :col-type text :initarg :subjective)
   ;; "Her"/"Him"
   (objective :col-type text :initarg :objective)
   ;; "Her"/"His"
   (possessive :col-type text :initarg :possessive))
  (:keys subjective)
  (:unique-index subjective)
  (:unique subjective))

(defun add-pronoun (subjective objective possessive &optional pluralp)
  (with-db ()
    (make-dao 'pronoun
              :pluralp pluralp
              :possessive possessive
              :subjective subjective :objective objective))
  t)

(defun remove-pronoun (text-id)
  (with-db ()
    (query (:delete-from 'pronoun :where (:= 'text-id text-id))))
  t)

;;; Adverbs
(defdao adverb ()
  ((id :col-type serial :reader id)
   (text :col-type text :initarg :text))
  (:keys text)
  (:unique-index text)
  (:unique text))

(defun add-adverb (adverb)
  (with-db () (make-dao 'adverb :text adverb))
  t)
(defun remove-adverb (adverb)
  (with-db () (query (:delete-from 'adverb :where (:= 'text adverb))))
  t)

;;; Verbs
;;;
;;; Using terminology from the wikipedia article on English Conjugation:
;;; https://secure.wikimedia.org/wikipedia/en/wiki/English_conjugation
(defdao verb ()
  ((id :col-type serial :reader id)
   (bare :col-type text :initarg :bare)
   (third-person :col-type text :initarg :third-person)
   (preterite :col-type text :initarg :preterite))
  (:keys bare)
  (:unique-index bare)
  (:unique bare))

(defun add-verb (bare &optional third-person preterite)
  (with-db ()
    (make-dao 'verb
              :bare bare
              :third-person (cond (third-person)
                                  ((or (ends-with #\x bare)
                                       (ends-with #\s bare))
                                   (concatenate 'string bare "es"))
                                  (t (concatenate 'string bare "s")))
              :preterite (cond (preterite)
                               ((ends-with #\e bare)
                                (concatenate 'string bare "d"))
                               (t (concatenate 'string bare "ed")))))
  t)
(defun remove-verb (bare)
  (with-db () (query (:delete-from 'verb :where (:= 'bare bare))))
  t)

(defun verb-completions (incomplete-verb)
  (with-db ()
    (query (:select 'third-person :from 'verb :where
                    (:ilike 'third-person (format nil "%~A%" incomplete-verb)))
           :column)))

;;; Testing
(defparameter *test-data*
  '((add-pronoun . (("she" "her" "her")
                    ("he" "him" "his")
                    ("it" "it" "its")
                    ("they" "them" "their" t)))
    (add-adverb . (("despondently") ("curiously") ("victoriously") ("amusedly")
                   ("sunnily") ("brightly") ("happily") ("honestly") ("nicely")
                   ("handsomely") ("cleverly") ("fascetiously") ("excitedly")
                   ("smugly") ("smilingly") ("angrily")))
    (add-verb . (("grin" "grins" "grinned") ("chuckle") ("fluff") ("squee")
                 ("pout") ("cackle") ("fix") ("preen") ("smile") ("frown")
                 ("cheer") ("laugh") ("wave") ("get" "gets" "got")
                 ("cry" "cries" "cried")))))

(defun reset ()
  (map nil #'rebuild-table '(verb adverb pronoun)))

(defun import-test-data ()
  (with-db ()
    (loop for (adder . args-list) in *test-data*
       do (loop for args in args-list
             do (apply adder args)))))
