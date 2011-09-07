(cl:defpackage #:sykosomatic.parser
  (:use :cl :alexandria :smug :sykosomatic.util)
  (:export :parse-input))
(cl:in-package #:sykosomatic.parser)

(optimizations)

(defun parse-input (actor input)
  (let ((result (car (invoke-parser (either (input) 'error) input))))
    (cond ((typep result 'error)
           (sykosomatic.websocket::send-msg actor (list "parse-error" (princ-to-string result))))
          ((null result)
           (sykosomatic.websocket::send-msg actor (list "parse-error" "Sorry, I couldn't understand what you said.")))
          (t
           (case (car result)
             (:dialogue
              (map nil (rcurry #'sykosomatic.websocket::send-dialogue actor
                               (cdr (assoc :dialogue (cdr result)))
                               (cdr (assoc :parenthetical (cdr result))))
                   (sykosomatic.websocket::local-actors actor)))
             (:sentence
              (map nil (rcurry #'sykosomatic.websocket::send-action actor
                               (sentence->text (cdr result)))
                   (sykosomatic.websocket::local-actors actor)))
             (:action
              (map nil (rcurry #'sykosomatic.websocket::send-action actor
                               (cdr result))
                   (sykosomatic.websocket::local-actors actor)))
             (:actorless-action
              (map nil (rcurry #'sykosomatic.websocket::send-action nil
                               (cdr result))
                   (sykosomatic.websocket::local-actors actor)))
             (:transition
              (map nil (rcurry #'sykosomatic.websocket::send-transition (cdr result))
                   (sykosomatic.websocket::local-actors actor)))
             (:ooc
              (map nil (rcurry #'sykosomatic.websocket::send-ooc actor (cdr result))
                   (sykosomatic.websocket::local-actors actor))))))))

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

(defvar *commands* (make-hash-table :test #'equalp))
(defvar *command-char* #\/)
(defun command ()
  (=let* ((_ (=char *command-char*))
          (command-name (text (=or (=satisfies #'alphanumericp) (=char #\-))))
          (command-arg (maybe (=and (one-or-more (whitespace))
                                    (text)))))
    (if-let (command-parser (find-command command-name))
      (result (caar (funcall command-parser (or command-arg ""))))
      (fail :error "No such command."))))

(defun add-command (name parser)
  (setf (gethash (string name) *commands*) parser))
(defun find-command (name)
  (gethash (string name) *commands*))
(defun remove-command (name)
  (remhash (string name) *commands*))
(defmacro defcommand (name-or-names () &body body)
  (let ((names (ensure-list name-or-names)))
    `(let ((cmd (funcall (lambda ()
                           ,@body))))
       ,@(loop for name in names
            collect `(add-command ,(if (stringp name) name (string name))
                                  cmd)))))

(defcommand (me em) ()
  (=let* ((action-text (text)))
    (result `(:action . ,action-text))))

(defcommand (action act) ()
  (=let* ((action-text (text)))
    (result `(:actorless-action . ,action-text))))

(defcommand (transition trans) ()
  (=let* ((text (text)))
    (result `(:transition . ,text))))

(defcommand ooc ()
  (=let* ((text (text)))
    (result `(:ooc . ,text))))

(defcommand error ()
  (=or
   (=let* ((text (text)))
     (fail :error text))
   (fail :error "Kaboom")))

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
  (=or (=let* ((_ (=and (=char #\@) (zero-or-more (whitespace))))
               (name (dashed-word)))
         (result `(:parenthetical . ,(concatenate 'string "to " name))))
       (=let* ((_ (=char #\())
               ;; TODO - Temporary while testing.
               (content (text (=and (=not (=char #\))) (item))) #+nil(=or (to/at-someone) (adverb)))
               (_ (=char #\))))
         ;; TODO - This'll need to return ,(cdr content) when the above is uncommented.
         (result `(:parenthetical . ,content)))))

(defun to/at-someone ()
  (=let* ((at/to (=or (=string "at")
                      (=string "to")))
          (_ (ws))
          (target-name (dashed-word)))
    (result `(:at/to . ,(concatenate 'string at/to " " target-name)))))

;; adverb = dashed-word that satisfies adverbp
(defun adverb ()
  (=let* ((dashed-word (text (alpha-char))))
    (if (adverbp dashed-word)
        (result `(:adverb . ,dashed-word))
        (fail :error (format nil "'~A' is not an adverb." dashed-word)))))

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

;; adjective = A dashed-word. Later validated against the noun it's attached to.
(defun adjective ()
  (dashed-word))

;; noun = A dashed-word that identifies a present, visible object.
(defun noun ()
  (dashed-word))

;; possessive-noun = noun ("'"/"'s")
(defun possessive-noun ()
  (=let* ((name (text (alpha-char)))
          (_ (=and (=char #\') (maybe (=char #\s)))))
    (result (cons :possessive name))))

(defun dashed-word ()
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
