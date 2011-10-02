(util:def-file-package #:sykosomatic.parser
  (:use :smug
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.game-objects.nameable)
  (:export :parse-action-completions :parse-dialogue :parse-action))

;;;
;;; Util
;;;
(defun invoke-parser (parser string)
  (mapcar #'car (funcall parser string)))

(defun dashed-word ()
  (text (=or (alpha-char) (=char #\-))))

;; ws = one or more whitespace
(defun ws ()
  (one-or-more (whitespace)))

;; adverb = dashed-word that satisfies adverbp
(defun adverb ()
  (=let* ((adverb (dashed-word)))
    (if (adverbp adverb)
        (result `(:adverb . ,adverb))
        (fail :error (format nil "'~A' is not an adverb." adverb)))))

;;;
;;; Dialogue
;;;
(defun to/at-someone ()
  (=let* ((at/to (=or (=string "at")
                      (=string "to")
                      (=string "@")))
          (_ (ws))
          (target-name (dashed-word)))
    (result `(:at/to . ,(concatenate 'string at/to " " target-name)))))

;; parenthetical = "(" ("@"|"to"|"at") name | adverb ")"
(defun parenthetical ()
  (=let* ((_ (=char #\())
          (content (=or (to/at-someone) (adverb)))
          (_ (=char #\))))
    (result `(:parenthetical . ,(cdr content)))))

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

(defun parse-dialogue (message)
  (let ((results (invoke-parser (either (dialogue) 'error) message)))
    (cond ((cdr results)
           (error "Parse was ambiguous."))
          ((typep (car results) 'error)
           (signal (car results)))
          (t (values (cdr (assoc :dialogue (cdar results)))
                     (cdr (assoc :parenthetical (cdar results))))))))

;;;
;;; Full-sentence action parser
;;;
;; verb = existing verb
(defun verb ()
  (=let* ((text (text (alpha-char))))
    (if-let (verb (find-verb text))
      (result verb)
      (fail :error (format nil "'~A' is not a verb." text)))))

(defun phrase-with-spaces ()
  (=let* ((word (dashed-word))
          (other-words (zero-or-more (=and (ws) (dashed-word)) #'plus)))
    (result (format nil "~A~{ ~A~}" word other-words))))

(defun noun-clause ()
  ;; This is only for testing/development.
  (=let* ((full-name (phrase-with-spaces)))
    (if-let (entity (find-by-full-name full-name))
      (result (list entity))
      (fail))))

(defun preposition (verb)
  (=let* ((prep (phrase-with-spaces)))
    (if (find prep (verb-prepositions verb) :test #'string-equal)
        (result prep)
        (fail))))

(defun transitive-verb-args (verb)
  (if (not (verb-transitive-p verb))
      (fail)
      (=let* ((_ (ws))
              (direct-objects (noun-clause))
              ;; Transitive verbs can have an adverb right in here.
              (adverb (maybe (=and (ws) (adverb)) 'error))
              (preposition (maybe (=prog2 (ws) (preposition verb) (ws))))
              (indirect-objects (if preposition
                                    (noun-clause)
                                    (result nil))))
        (result `((:adverb . ,(cdr adverb))
                  (:direct-objects . ,direct-objects)
                  (:preposition . ,preposition)
                  (:indirect-objects . ,indirect-objects))))))

(defun ditransitive-verb-args (verb)
  (if (not (verb-ditransitive-p verb))
      (fail)
      (=or
       (=let* ((_ (ws))
               (direct-objects (noun-clause))
               (preposition (=prog2 (ws) (=string "to") (ws)))
               (indirect-objects (noun-clause)))
         (result `((:direct-objects . ,direct-objects)
                   (:indirect-objects . ,indirect-objects)
                   (:preposition . ,preposition))))
       (=let* ((_ (ws))
               (indirect-objects (noun-clause))
               (_ (ws))
               (direct-objects (noun-clause)))
         (result `((:direct-objects . ,direct-objects)
                   (:indirect-objects . ,indirect-objects)))))))

(defun intransitive-verb-args (verb)
  (if (not (verb-intransitive-p verb))
      (fail)
      (=let* ((preposition (maybe (=prog2 (ws) (preposition verb) (ws))))
              (indirect-objects (if preposition
                                    (noun-clause)
                                    (result nil))))
        (result `((:indirect-objects . ,indirect-objects)
                  (:preposition . ,preposition))))))

(defun verb-args (verb)
  (=or (transitive-verb-args verb)
       (ditransitive-verb-args verb)
       (intransitive-verb-args verb)))

;; sentence = [adverb ws] verb [ws adverb] [ws verb-args] [ws adverb]
(defun sentence ()
  (=let* ((adverb1 (maybe (=prog1 (adverb) (ws)) 'error))
          (verb (verb))
          (adverb2 (maybe (=and (ws) (adverb)) 'error))
          (verb-args (verb-args verb))
          (adverb3 (maybe (=and (ws) (adverb)) 'error)))
    (result `(:sentence
              ,@(remove :adverb verb-args :key #'car)
              (:adverbs . ,(list (cdr adverb1)
                                 (cdr adverb2)
                                 (cdr (assoc :adverb verb-args))
                                 (cdr adverb3)))
              (:verb . ,(verb-third-person verb))))))

;;; Commands
(defun parse-action (actor message)
  (let ((results (invoke-parser (either (=prog1 (sentence) (no-more-input)) 'error) message)))
    (cond ((null results)
           (error "ENOPARSE"))
          ((cdr results)
           (error "Parse was ambiguous."))
          ((typep (car results) 'error)
           (error (car results)))
          (t (let ((sentence (cdar results)))
               (apply #'invoke-verb-command :actor actor (alist-plist sentence)))))))

;;;
;;; Partial-sentence completion
;;;
(defparameter *max-completion-results* 25)

(defun parse-action-completions (action-text)
  (when-let (results (funcall (either (=prog1 (action-completions) (no-more-input))
                                      'error) action-text))
    (unless
        (<= *max-completion-results*
            (reduce #'* results :key (compose #'length #'car))))
    (loop for (completions . leftovers) in results
       when (typep completions 'error)
       do (error completions)
       when (emptyp leftovers)
       append completions)))

(defun action-completions ()
  (=let* ((completions1 (partial-verb-or-adverb))
          (completions2 (maybe (=and (ws)
                                     (if (eq :verbs (car completions1))
                                         (partial-adverb)
                                         (partial-verb))))))
    (cond ((<= *max-completion-results*
               (* (length (cdr completions1))
                  (length (cdr completions2))))
           (fail))
          ((and completions1 completions2)
           (result
            (loop for c1 in (cdr completions1)
               appending (loop for c2 in (cdr completions2)
                            collect (concatenate 'string c1 " " c2)))))
          (completions1
           (result (cdr completions1)))
          (t
           (fail)))))

(defun partial-verb-or-adverb ()
  (plus
   (=let* ((completions (partial-verb)))
     (if completions
         (result `(:verbs . ,(cdr completions)))
         (fail)))
   (=let* ((completions (partial-adverb)))
     (if completions
         (result `(:adverbs . ,(cdr completions)))
         (fail)))))

(defun partial-vocabulary-word (search-fun)
  (=let* ((partial-word (dashed-word)))
    (if-let (completed (funcall search-fun partial-word))
      (result `(:completions . ,completed))
      (fail))))

(defun partial-verb ()
  (partial-vocabulary-word #'verb-completions))
(defun partial-adverb ()
  (partial-vocabulary-word #'adverb-completions))

;; #+nil(defun sentence ()
;;   (=let* ((adverb1 (maybe (=prog1 (adverb) (ws)) 'error))
;;           (verb (verb))
;;           (noun-clause-1 (maybe (=and (ws) (noun-clause)) 'error))
;;           (noun-clause-2 (maybe (=and (ws) (noun-clause)) 'error))
;;           (adverb2 (maybe (=and (ws) (adverb)) 'error)))
;;     (result `(:sentence
;;               (:adverb1 . ,adverb1)
;;               (:verb . ,verb)
;;               (:noun-clause-1 . ,noun-clause-1)
;;               (:noun-clause-2 . ,noun-clause-2)
;;               (:adverb2 . ,adverb2)))))

;; ;; noun-clause = [[[adverb ws] preposition ws] noun-group]
;; (defun noun-clause ()
;;   ;; TODO
;;   (noun-group))

;; ;; noun-group =  noun-phrase 0*(["," ws] conjunction noun-phrase)
;; (defun noun-group ()
;;   (=let* ((x (noun-phrase))
;;           (y (zero-or-more (conjed-noun-phrase))))
;;     (result (cons x y))))
;; (defun conjed-noun-phrase ()
;;   (=let* ((comma (maybe (=and (=char #\,) (ws))))
;;           (_ (if comma (maybe (=prog1 (conjunction) (ws))) (=prog2 (ws) (conjunction) (ws))))
;;           (np (noun-phrase)))
;;     (result np)))

;; ;; conjunction = "and" / "&"
;; (defun conjunction ()
;;   (=or (=string "and")
;;        (=string "&")))

;; ;; noun-phrase = [cardinal ws] 0*(adjective ws) noun
;; ;; noun-phrase =/ [article ws] [ordinal ws] 0*(adjective ws) \
;; ;;                (noun / possessive-noun ws noun-phrase)
;; (defun noun-phrase ()
;;   (=or (=let* ((cardinal (=prog1 (cardinal) (ws)))
;;                (adjective (zero-or-more (=prog1 (adjective) (ws))))
;;                (noun (noun)))
;;          (result (list :noun-phrase
;;                        :cardinal cardinal
;;                        :adjectives adjective
;;                        :noun noun)))
;;        (=let* ((article (maybe (=prog1 (article) (ws))))
;;                (ordinal (=and (=not (cardinal)) (maybe (=prog1 (ordinal) (ws)))))
;;                (adjective (zero-or-more (=prog1 (adjective) (ws))))
;;                (noun (noun)))
;;          ;; TODO - What this should -really- return is the id of the game object, itself. If it
;;          ;;        fails to bind to an object, then the parser itself can fail. This could be used
;;          ;;        to stop the all-powerful hunger of the adjective parser.
;;          (result (list :noun-phrase
;;                        :article article
;;                        :ordinal ordinal
;;                        :adjectives adjective
;;                        :noun noun)))))

;; ;; article = "the" / "a"
;; (defun article ()
;;   (=or (=string "the")
;;        (=string "a")))

;; ;; adjective = A dashed-word. Later validated against the noun it's attached to.
;; (defun adjective ()
;;   (dashed-word))

;; ;; noun = A dashed-word that identifies a present, visible object.
;; (defun noun ()
;;   (dashed-word))

;; ;; possessive-noun = noun ("'"/"'s")
;; (defun possessive-noun ()
;;   (=let* ((name (text (alpha-char)))
;;           (_ (=and (=char #\') (maybe (=char #\s)))))
;;     (result (cons :possessive name))))

;; (defun nounp (maybe-noun)
;;   maybe-noun)
