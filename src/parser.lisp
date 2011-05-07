(cl:defpackage #:sykosomatic.parser
  (:use :cl :alexandria :smug :sykosomatic.utils)
  (:export :parse-input))
(cl:in-package #:sykosomatic.parser)

(defun parse-input (actor input)
  (let ((result (car (invoke-parser (maybe (input) 'error) input))))
    (cond ((typep result 'error)
           (sykosomatic::send-msg actor (list "parse-error" (princ-to-string result))))
          ((null result)
           (sykosomatic::send-msg actor (list "parse-error" "Sorry, I couldn't understand what you said.")))
          (t
           (case (car result)
             (:dialogue
              (map nil (rcurry #'sykosomatic::send-dialogue actor
                               (cdr (assoc :dialogue (cdr result)))
                               (cdr (assoc :parenthetical (cdr result))))
                   (sykosomatic::local-actors actor)))
             (:sentence
              (map nil (rcurry #'sykosomatic::send-action actor
                               (sentence->text (cdr result)))
                   (sykosomatic::local-actors actor))))))))

(defun sentence->text (sentence)
  (format nil "~A." (cdr (assoc :verb sentence))))

(defun invoke-parser (parser string)
  (mapcar #'car (funcall parser string)))

;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------

;; input = dialogue / action
(defun input ()
  (=or (action)
       (dialogue)))

;; ws = zero or more whitespace
(defun ws ()
  (one-or-more (whitespace)))

;; dialogue = [parenthetical ws] text
(defun dialogue ()
  (=let* ((parenthetical (maybe (=prog1 (parenthetical) (ws))))
          (dialogue-text (if parenthetical
                             (text)
                             (=and (=not (=char #\())
                                   (text)))))
    (result `(:dialogue
              (:parenthetical . ,(cdr parenthetical))
              (:dialogue . ,dialogue-text)))))

;; parenthetical = "(" adverb ")"
(defun parenthetical ()
  (=let* ((_ (=char #\())
          (content (adverb))
          (_ (=char #\))))
    (result `(:parenthetical . ,(cdr content)))))

;; adverb = word that satisfies adverbp
(defun adverb ()
  (=let* ((word (text (alpha-char))))
    (if (adverbp word)
        (result `(:adverb . ,word))
        (fail :error (format nil "'~A' is not an adverb." word)))))

;; action = action-delimiter 0*ws sentence
(defun action ()
  (=and (action-delimiter)
        (zero-or-more (whitespace))
        (sentence)))

;; action-delimiter = "*"
(defun action-delimiter ()
  (=or (=char #\*)
       (=string "/me ")
       (=string "/em ")
       (=string "/act ")))

;; sentence = [adverb ws] verb [ws noun-clause] [ws noun-clause] [ws adverb]
(defun sentence ()
  (=let* ((verb (verb))
          (_ (maybe (=char #\.))))
    (result `(:sentence
              (:verb . ,verb)))))

#+nil(defun sentence ()
  (=let* ((adverb1 (maybe (=prog1 (adverb) (ws)) 'error))
          (verb (verb))
          (noun-clause-1 (maybe (=and (ws) (noun-clause)) 'error))
          (noun-clause-2 (maybe (=and (ws) (noun-clause)) 'error))
          (adverb2 (maybe (=and (ws) (adverb)) 'error)))
    (result `(:sentence
              (:adverb1 . ,adverb1)
              (:verb . ,verb)
              (:noun-clause-1 . ,noun-clause-1)
              (:noun-clause-2 . ,noun-clause-2)
              (:adverb2 . ,adverb2)))))

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

;; verb = existing verb
(defun verb ()
  (=let* ((text (text (alpha-char))))
    (if (verbp text)
        (result text)
        (fail :error (format nil "'~A' is not a verb." text)))))

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

;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)
(defun conjunction ()
  (=or (=string "and")
       (=string "&")
       (=string ",")))

;;;
;;; Word identifiers
;;;
(defun nounp (maybe-noun)
  (find maybe-noun '("flask") :test #'string-equal))

(defun adverbp (maybe-adverb)
  (find maybe-adverb '("amusedly" "sunnily" "brightly" "happily" "honestly" "nicely" "handsomely" "cleverly" "fascetiously") :test #'string-equal))

(defun verbp (maybe-verb)
  (find maybe-verb '("smiles" "frowns" "cheers" "laughs" "cries" "waves") :test #'string-equal))
