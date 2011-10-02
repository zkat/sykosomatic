(util:def-file-package #:sykosomatic.vocabulary
  (:use :sykosomatic.db)
  (:export :add-pronoun :remove-pronoun :add-adverb :remove-adverb :adverbp
           :add-verb :remove-verb :find-verb :verbp :verb-completions :adverb-completions
           :third-person-singular :preterite))

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
;;;
;;; Also see:
;;; https://secure.wikimedia.org/wikipedia/en/wiki/Transitive_verb
;;; https://secure.wikimedia.org/wikipedia/en/wiki/Intransitive_verb
;;; https://secure.wikimedia.org/wikipedia/en/wiki/Ditransitive_verb
(defdao verb ()
  ((bare text)
   (third-person text)
   (preterite text)
   (transitivep boolean :col-default nil)
   (intransitivep boolean :col-default nil)
   (ditransitivep boolean :col-default nil))
  (:keys id)
  (:unique-index third-person)
  (:unique third-person))

(defun add-verb (bare &key third-person preterite
                 transitivep intransitivep ditransitivep)
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
                               (t (concatenate 'string bare "ed")))
              :intransitivep intransitivep
              :transitive transitivep
              :distransitivep ditransitivep))
  t)

(defun remove-verb (bare)
  (db-query (:delete-from 'verb :where (:= 'bare bare)))
  t)

(defun verb-completions (incomplete-verb &optional (limit 50))
  (db-query (:limit (:select 'third-person :from 'verb :where
                             (:ilike 'third-person (format nil "%~A%" incomplete-verb)))
                    limit)
            :column))

(defun find-verb (third-person)
  (db-query (:select :* :from 'verb :where (:ilike 'third-person third-person))
            :plist))

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
    (add-verb . (("grin" :third-person "grins" :preterite "grinned" :intransitivep t)
                 ("chuckle" :intransitivep t) ("fluff" :intransitivep t :transitivep t)
                 ("squee" :intransitivep t) ("pout" :transitivep t :intransitivep t)
                 ("cackle" :intransitivep t) ("fix" :transitivep t) ("preen" :intransitivep t)
                 ("smile" :intransitivep t) ("frown" :intransitivep t)
                 ("cheer" :transitivep t :intransitivep t) ("laugh" :intransitivep t)
                 ("wave" :transitivep t :intransitivep t) ("get" :preterite "got" :transitivep t)
                 ("cry" :third-person "cries" :preterite "cried" :intransitivep t)
                 ("punch" :transitivep t) ("give" :preterite "gave" :ditransitivep t)))))

(defun reset ()
  (map nil #'rebuild-table '(verb adverb pronoun)))

(defun import-test-data ()
  (loop for (adder . args-list) in *test-data*
     do (loop for args in args-list
           do (apply adder args))))

(register-db-init-hook 'vocabulary-init 'import-test-data)
