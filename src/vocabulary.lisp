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
   (ditransitivep boolean :col-default nil)
   (prepositions text[]))
  (:keys id)
  (:unique-index third-person)
  (:unique third-person))

(defun add-verb (bare &key third-person preterite prepositions
                 transitivep intransitivep ditransitivep)
  (with-db ()
    (make-dao 'verb
              :bare bare
              :third-person (cond (third-person)
                                  ((or (ends-with #\x bare)
                                       (ends-with #\s bare)
                                       (ends-with-subseq "ch" bare))
                                   (concatenate 'string bare "es"))
                                  (t (concatenate 'string bare "s")))
              :preterite (cond (preterite)
                               ((ends-with #\e bare)
                                (concatenate 'string bare "d"))
                               (t (concatenate 'string bare "ed")))
              :transitivep transitivep
              :ditransitivep ditransitivep
              :intransitivep intransitivep
              :prepositions (if prepositions
                                (coerce prepositions 'array)
                                '(:raw "ARRAY[]::text[]"))))
  t)

(defun remove-verb (third-person)
  (db-query (:delete-from 'verb :where (:= 'third-person third-person)))
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
    (add-verb . (("grin" :third-person "grins" :preterite "grinned" :intransitivep t
                         :prepositions ("at" "with"))
                 ("chuckle" :intransitivep t :prepositions ("at" "with"))
                 ("fluff" :intransitivep t :transitivep t
                          :prepositions ("at" "with"))
                 ("squee" :intransitivep t :prepositions ("at" "with"))
                 ("pout" :transitivep t :intransitivep t
                         :prepositions ("at" "with"))
                 ("cackle" :intransitivep t :prepositions ("at" "with"))
                 ("fix" :transitivep t :prepositions ("with"))
                 ("preen" :intransitivep t :prepositions ("at" "with"))
                 ("smile" :intransitivep t :prepositions ("at" "with"))
                 ("frown" :intransitivep t :prepositions ("at" "with"))
                 ("cheer" :transitivep t :intransitivep t :prepositions ("at" "with"))
                 ("laugh" :intransitivep t :prepositions ("at" "with"))
                 ("wave" :transitivep t :intransitivep t :prepositions ("at" "to" "with"))
                 ("get" :preterite "got" :transitivep t :prepositions ("at" "to" "with"))
                 ("cry" :third-person "cries" :preterite "cried" :intransitivep t
                        :prepositions ("at" "to" "with" "over" "on"))
                 ("punch" :transitivep t :prepositions ("at" "with"))
                 ("give" :preterite "gave" :ditransitivep t :prepositions ("to" "with"))
                 ("throw" :preterite "threw" :ditransitivep t :transitivep t
                          :prepositions ("to" "at" "with"))))))

(defun import-test-data ()
  (loop for (adder . args-list) in *test-data*
     do (loop for args in args-list
           do (apply adder args))))

(defun reset ()
  (map nil #'rebuild-table '(verb adverb pronoun))
  (import-test-data))

(register-db-init-hook 'vocabulary-init 'import-test-data)
