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
               (chat (maybe (chat-string))))
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
;; noun-phrase =/ [article] [cardinal] [adjective] noun
;; noun-phrase =/ [article] [ordinal] [adjective] \
;;                (noun / possessive-noun noun-phrase)
(defun noun-phrase ()
  (=or (pronoun)
       (=let* ((article (maybe (article)))
               (cardinal (cardinal))
               (adjective (maybe (adjective)))
               (noun (noun)))
         (result (list :noun-phrase
                       :article article
                       :cardinal cardinal
                       :adjective adjective
                       :noun noun)))
       (=let* ((article (maybe (article)))
               (ordinal (maybe (ordinal)))
               (adjective (maybe (adjective)))
               (noun (=or (possessive-noun-phrase)
                          (noun))))
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
  ;; TODO
  nil)

;; adjective = any unknown token that comes before a noun or a possessive
(defun adjective ()
  ;; TODO
  nil)
;; noun = anything that identifies a present, visible object (oof?)
(defun noun ()
  (word #'nounp))

;; pronoun = satisfies pronoun-p
(defun pronoun ()
  ;; TODO
  nil)

;; possessive-noun = satisfies possessive-p (['s] or [s'])
(defun possessive-noun ()
  ;; TODO
  nil)

;; cardinal = 1, 2, three, four...
(defun cardinal ()
  ;; TODO
  nil)

;; ordinal = 1st, 2nd, third, fourth....
(defun ordinal ()
  ;; TODO
  nil)

;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)
(defun conjunction ()
  (=or (=let* ((_ (=and (=char #\,)
                        (maybe (word (curry #'string-equal "and"))))))
         (result ","))
       (=or (word (curry #'string-equal "and")))))

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
  (=let* ((word (skip-whitespace (text (none-of '(#\space #\tab #\,))))))
    (if (funcall test word)
        (result word)
        (fail))))
