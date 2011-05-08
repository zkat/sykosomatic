(cl:defpackage #:sykosomatic.parser
  (:use :cl :alexandria :smug :sykosomatic.utils)
  (:export :parse-input))
(cl:in-package #:sykosomatic.parser)

(defun parse-input (actor input)
  (let ((result (car (invoke-parser (either (input) 'error) input))))
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

;; input = command / dialogue
(defun input ()
  (=or (command)
       (dialogue)))

(defun command ()
  (action))

;; ws = one or more whitespace
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
;; parenthetical =/ "@" name
(defun parenthetical ()
  (=or (=let* ((_ (=char #\@))
               (name (word)))
         (result `(:parenthetical . ,(concatenate 'string "to " name))))
       (=let* ((_ (=char #\())
               (content (=or (to/at-someone) (adverb)))
               (_ (=char #\))))
         (result `(:parenthetical . ,(cdr content))))))

(defun to/at-someone ()
  (=let* ((at/to (=or (=string "at")
                      (=string "to")))
          (_ (ws))
          (target-name (word)))
    (result `(:at/to . ,(concatenate 'string at/to " " target-name)))))

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
  (=or (=string "/me ")
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

;; noun-clause = [[[adverb ws] preposition ws] noun-group]
(defun noun-clause ()
  ;; TODO
  (noun-group))

;; noun-group =  noun-phrase 0*(["," ws] conjunction noun-phrase)
(defun noun-group ()
  (=let* ((x (noun-phrase))
          (y (zero-or-more (conjed-noun-phrase))))
    (result (cons x y))))
(defun conjed-noun-phrase ()
  (=let* ((comma (maybe (=and (=char #\,) (ws))))
          (_ (if comma (maybe (=prog1 (conjunction) (ws))) (=prog2 (ws) (conjunction) (ws))))
          (np (noun-phrase)))
    (result np)))

;; conjunction = "and" / "&"
(defun conjunction ()
  (=or (=string "and")
       (=string "&")))

;; noun-phrase = [cardinal ws] 0*(adjective ws) noun
;; noun-phrase =/ [article ws] [ordinal ws] 0*(adjective ws) \
;;                (noun / possessive-noun ws noun-phrase)
(defun noun-phrase ()
  (=or (=let* ((cardinal (=prog1 (cardinal) (ws)))
               (adjective (zero-or-more (=prog1 (adjective) (ws))))
               (noun (noun)))
         (result (list :noun-phrase
                       :cardinal cardinal
                       :adjectives adjective
                       :noun noun)))
       (=let* ((article (maybe (=prog1 (article) (ws))))
               (ordinal (=and (=not (cardinal)) (maybe (=prog1 (ordinal) (ws)))))
               (adjective (zero-or-more (=prog1 (adjective) (ws))))
               (noun (noun)))
         ;; TODO - What this should -really- return is the id of the game object, itself. If it
         ;;        fails to bind to an object, then the parser itself can fail. This could be used
         ;;        to stop the all-powerful hunger of the adjective parser.
         (result (list :noun-phrase
                       :article article
                       :ordinal ordinal
                       :adjectives adjective
                       :noun noun)))))

;; verb = existing verb
(defun verb ()
  (=let* ((text (text (alpha-char))))
    (if (verbp text)
        (result text)
        (fail :error (format nil "'~A' is not a verb." text)))))

;; article = "the" / "a"
(defun article ()
  (=or (=string "the")
       (=string "a")))

;; adjective = A word. Later validated against the noun it's attached to.
(defun adjective ()
  (word))

;; noun = A word that identifies a present, visible object.
(defun noun ()
  (word))

;; possessive-noun = noun ("'"/"'s")
(defun possessive-noun ()
  (=let* ((name (text (alpha-char)))
          (_ (=and (=char #\') (maybe (=char #\s)))))
    (result (cons :possessive name))))

(defun word ()
  (text (=or (alpha-char) (=char #\-))))
;;;
;;; Word identifiers
;;;
(defun nounp (maybe-noun)
  (find maybe-noun '("flask") :test #'string-equal))

(defun adverbp (maybe-adverb)
  (find maybe-adverb
  '("despondently" "curiously" "victoriously" "amusedly"
    "sunnily" "brightly" "happily" "honestly" "nicely"
    "handsomely" "cleverly" "fascetiously" "excitedly"
    "smugly" "smilingly" "angrily")
  :test #'string-equal))

(defun verbp (maybe-verb)
  (find maybe-verb
  '("grins" "chuckles" "fluffs" "squees" "pouts" "cackles" "fixes"
    "preens" "smiles" "frowns" "cheers" "laughs" "cries" "waves")
  :test #'string-equal))
