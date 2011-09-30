(cl:defpackage #:sykosomatic.vocabulary
  (:use :cl :alexandria :sykosomatic.db)
  (:export :add-pronoun :remove-pronoun :add-adverb :remove-adverb :adverbp
           :add-verb :remove-verb :verbp :verb-completions :adverb-completions
           :third-person-singular :preterite))
(cl:in-package #:sykosomatic.vocabulary)

;;; Pronouns
(defdao pronoun ()
  ((pluralp boolean :col-default nil)
   ;; "She"/"He"
   (subjective text)
   ;; "Her"/"Him"
   (objective text)
   ;; "Her"/"His"
   (possessive text))
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
  (db-query (:delete-from 'pronoun :where (:= 'text-id text-id)))
  t)

;;; Adverbs
(defdao adverb ()
  ((text text))
  (:keys text)
  (:unique-index text)
  (:unique text))

(defun add-adverb (adverb)
  (with-db () (make-dao 'adverb :text adverb))
  t)
(defun remove-adverb (adverb)
  (db-query (:delete-from 'adverb :where (:= 'text adverb)))
  t)

(defun adverb-completions (incomplete-adverb &optional (limit 50))
  (db-query (:limit (:select 'text :from 'adverb :where
                             (:ilike 'text (format nil "%~A%" incomplete-adverb)))
                    limit)
            :column))

(defun adverbp (maybe-adverb)
  (db-query (:select t :from 'adverb :where (:ilike 'text maybe-adverb)) :single))

;;; Verbs
;;;
;;; Using terminology from the wikipedia article on English Conjugation:
;;; https://secure.wikimedia.org/wikipedia/en/wiki/English_conjugation
(defdao verb ()
  ((bare text)
   (third-person text)
   (preterite text))
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
  (db-query (:delete-from 'verb :where (:= 'bare bare)))
  t)

(defun verb-completions (incomplete-verb &optional (limit 50))
  (db-query (:limit (:select 'third-person :from 'verb :where
                             (:ilike 'third-person (format nil "%~A%" incomplete-verb)))
                    limit)
            :column))

(defun verbp (maybe-verb)
  (db-query (:select t :from 'verb :where (:ilike 'third-person maybe-verb)) :single))

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
  (loop for (adder . args-list) in *test-data*
     do (loop for args in args-list
           do (apply adder args))))
