(cl:defpackage #:sykosomatic.parser
  (:use :cl :alexandria :smug)
  (:export :parse-input))
(cl:in-package #:sykosomatic.parser)

(defun parse-input (actor input)
  (when-let ((dialogue (car (invoke-parser (dialogue) input))))
    (map nil (rcurry #'sykosomatic::send-dialogue actor dialogue)
         (sykosomatic::local-actors actor))))

(defun invoke-parser (parser string)
  (mapcar #'car (funcall parser string)))

;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------
;;
;; sentence =  dialogue
;; sentence =/ [adverb] verb [noun-clause] [noun-clause] [adverb] [dialogue]
;;
;; noun-clause =/ [[[adverb] preposition] noun-group]
;;
;; noun-group =  noun-phrase [","] 0*(conjunction noun-phrase)
;;
;; noun-phrase =  pronoun
;; noun-phrase =/ [article] [cardinal] [adjective] noun
;; noun-phrase =/ [article] [ordinal] [adjective] \
;;                (noun / possessive-noun noun-phrase)
;;
;; article = satisfies article-p
;; adjective = any unknown token that comes before a noun or a possessive
;; noun = anything before a preposition, a conjunction, an adverb, a dialogue, or a NIL
;; pronoun = satisfies pronoun-p
;; possessive-noun = satisfies possessive-p (['s] or [s'])
;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)

;; sentence =  dialogue
;; sentence =/ [adverb] verb [noun-clause] [noun-clause] [adverb] [dialogue]
(defun sentence ()
  (=or (dialogue)
       (=let* ((adverb1 (maybe (adverb)))
               (verb (verb))
               (noun-clause1 (maybe (noun-clause)))
               (noun-clause2 (maybe (noun-clause)))
               (adverb2 (maybe (adverb)))
               (chat (maybe (dialogue)))
               (_ (no-more-input)))
         (result (list :sentence
                       :adverbs (list adverb1 adverb2)
                       :verb verb
                       :noun-clause-1 noun-clause1
                       :noun-clause-2 noun-clause2
                       :dialogue chat)))))

;; noun-clause =/ [[[adverb] preposition] noun-group]
(defun noun-clause ()
  (noun-group))

;; noun-group =  noun-phrase [","] 0*(conjunction noun-phrase)
(defun noun-group ()
  (=let* ((x (noun-phrase))
          (y (zero-or-more (conjed-noun-phrase))))
    (result (cons x y))))
(defun conjed-noun-phrase ()
  (=let* ((_ (conjunction))
          (np (noun-phrase)))
    (result np)))

;; noun-phrase =  pronoun
;; noun-phrase =/ [cardinal] [adjective] noun
;; noun-phrase =/ [article] [ordinal] [adjective] \
;;                (noun / possessive-noun-phrase)
(defun noun-phrase ()
  (=or (pronoun)
       (=let* ((cardinal (cardinal))
               (adjective (maybe (adjective)))
               (noun (noun)))
         (result (list :noun-phrase
                       :cardinal cardinal
                       :adjective adjective
                       :noun noun)))
       (=let* ((article (maybe (article)))
               (ordinal (=and (=not (cardinal)) (maybe (ordinal))))
               (adjective (maybe (adjective)))
               (noun (noun)))
         (result (list :noun-phrase
                       :article article
                       :ordinal ordinal
                       :adjective adjective
                       :noun noun)))))

(defun possessive-noun-phrase ()
  ;; TODO
  nil)

(defun dialogue-delimiter ()
  (=or (=char #\")
       (=char #\')))

(defun dialogue ()
  (=let* ((_ (dialogue-delimiter))
          (text (text))
          (_ (=or (=and (dialogue-delimiter)
                        (no-more-input))
                  (no-more-input))))
    (result (if (char= (char text (1- (length text)))
                       #\")
                (subseq text 0 (1- (length text)))
                text))))

;; adverb = existing adverb
(defun adverb ()
  (word #'adverbp))

;; verb = existing verb
(defun verb ()
  (word #'verbp))

;; article = satisfies articlep
(defun article ()
  (=or (=string "the")
       (=string "a")))

;; adjective = any unknown token that comes before a noun or a possessive
(defun adjective ()
  ;; TODO
  (fail))

;; noun = anything that identifies a present, visible object (oof?)
(defun noun ()
  (word))

;; pronoun = satisfies pronoun-p
(defun pronoun ()
  ;; TODO
  (fail))

;; possessive-noun = satisfies possessive-p (['s] or [s'])
(defun possessive-noun ()
  ;; TODO
  (fail))

;; cardinal = 1, 2, three, four...
(defun cardinal ()
  (=let* ((card (=or (=let* ((num (natural-number))
                             (_ (one-or-more (whitespace))))
                       (result num))
                     (word))))
    (if (numberp card)
        (result card)
        (let ((position (position card (loop for i from 1 upto 20
                                          collect (format nil "~r" i)) :test #'string-equal)))
          (if position
              (result (1+ position))
              (fail))))))

;; ordinal = 1st, 2nd, third, fourth....
(defun ordinal ()
  ;; TODO - 1st, 2nd, ...
  (=let* ((ord (word)))
    (let ((position (position ord (loop for i from 1 upto 20
                                     collect (format nil "~:r" i)) :test #'string-equal)))
      (if position
          (result (1+ position))
          (fail)))))

;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)
(defun conjunction ()
  (=or (skip-whitespace (=string "and"))
       (skip-whitespace (=string "&"))
       #+nil(=let* ((_ (=and (=char #\,)
                        (maybe (word (curry #'string-equal "and"))))))
         (result ","))
       #+nil(=or (word (curry #'string-equal "and"))
            (word (curry #'string-equal "&")))))

;;;
;;; Word identifiers
;;;
(defun nounp (maybe-noun)
  (find maybe-noun '("flask") :test #'string-equal))

(defun adverbp (maybe-adverb)
  (find maybe-adverb '("handsomely" "cleverly" "fascetiously") :test #'string-equal))

(defun verbp (maybe-verb)
  (string= maybe-verb "get"))
