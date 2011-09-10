(cl:defpackage #:sykosomatic.vocabulary
  (:use :cl :alexandria :postmodern :sykosomatic.db)
  (:export :add-pronoun :remove-pronoun :add-adverb :remove-adverb
           :add-verb :remove-verb :third-person-singular :preterite))
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
;;;
;;; For the purposes of the parser, etc, we assume that we can just conjugate them, unless there's a
;;; value present in the irregular-blah fields.
(defdao verb ()
  ((id :col-type serial :reader id)
   (bare :col-type text :initarg :bare)
   (irregular-third-person :col-type (or db-null text) :initarg :irregular-third-person)
   (irregular-preterite :col-type (or db-null text) :initarg :irregular-preterite))
  (:keys bare)
  (:unique-index bare)
  (:unique bare))

(defun add-verb (bare &optional irregular-third-person irregular-preterite)
  (with-db () (make-dao 'verb
                        :bare bare :irregular-preterite (or irregular-preterite :null)
                        :irregular-third-person (or irregular-third-person :null)))
  t)
(defun remove-verb (bare)
  (with-db () (query (:delete-from 'verb :where (:= 'bare bare))))
  t)

(defun third-person-singular (verb)
  "If VERB is a bare-form verb listed in the database, this function returns it in the third person
singular form."
  (when-let (from-db (with-db () (query (:select 'irregular-third-person :from 'verb
                                                 :where (:= 'bare verb))
                                        :single)))
    (cond ((stringp from-db) from-db)
          ((or (ends-with #\x verb)
               (ends-with #\s verb))
           (concatenate 'string verb "es"))
          (t (concatenate 'string verb "s")))))

(defun preterite (verb)
  "If VERB is a bare-form verb listed in the database, this function returns it in preterite form."
  (when-let (from-db (with-db () (query (:select 'irregular-preterite :from 'verb
                                                 :where (:= 'bare verb))
                                        :single)))
    (cond ((stringp from-db) from-db)
          ((ends-with #\e verb)
           (concatenate 'string verb "d"))
          (t
           (concatenate 'string verb "ed")))))

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
                 ("cheer") ("laugh") ("wave")
                 ("cry" "cries" "cried")))))

(defun import-test-data ()
  (with-db ()
    (loop for (adder . args-list) in *test-data*
       do (loop for args in args-list
             do (apply adder args)))))
