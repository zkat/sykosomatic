(cl:defpackage #:sykosomatic.parser
  (:use :cl :alexandria :smug))
(cl:in-package #:sykosomatic.parser)

;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------
;;
;; sentence =  chat-string
;; sentence =/ [adverb] verb [noun-clause] [noun-clause] [adverb] [chat-string]
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
;; noun = anything before a preposition, a conjunction, an adverb, a chat-string, or a NIL
;; pronoun = satisfies pronoun-p
;; possessive-noun = satisfies possessive-p (['s] or [s'])
;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)

;; sentence =  chat-string
;; sentence =/ [adverb] verb [noun-clause] [noun-clause] [adverb] [chat-string]
(defun sentence ()
  (=or (chat-string)
       (=let* ((adverb1 (maybe (adverb)))
               (verb (verb))
               (noun-clause1 (maybe (noun-clause)))
               (noun-clause2 (maybe (noun-clause)))
               (adverb2 (maybe (adverb)))
               (chat (maybe (chat-string)))
               (_ (no-more-input)))
         (result (list :sentence
                       :adverbs (list adverb1 adverb2)
                       :verb verb
                       :noun-clause-1 noun-clause1
                       :noun-clause-2 noun-clause2
                       :chat-string chat)))))

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

(defun chat-string-delimiter ()
  (=or (=char #\")
       (=char #\')))

(defun chat-string ()
  (=let* ((_ (chat-string-delimiter))
          (text (text))
          (_ (=or (=and (chat-string-delimiter)
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

;;;
;;; Utils
;;;
(defun maybe (parser)
  (=or parser (result nil)))

(defun word (&optional (test #'identity))
  (=let* ((word (skip-whitespace (text (alpha-char)))))
    (if (funcall test word)
        (result word)
        (fail))))

(defun alpha-char ()
  (=satisfies #'alpha-char-p))
