(util:def-file-package #:sykosomatic.parser
  (:use :smug
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.components.describable)
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

(defun local-object (observer)
  (=let* ((identifier (phrase-with-spaces)))
    (if-let (entity (car (find-by-short-description observer identifier)))
      (result entity)
      (fail))))

(defun comma ()
  (=and
   (maybe (ws))
   (=char #\,)
   (maybe (ws))
   (result t)))

(defun conjunction ()
  (=or
   (=let* ((commap (maybe (comma)))
           (_ (if commap (result nil) (ws)))
           (_ (=string "and"))
           (_ (ws)))
     (result t))
   (comma)))

(defun noun-clause (observer)
  (=let* ((obj (local-object observer))
          (more-objects (zero-or-more (=and (conjunction)
                                            (local-object observer)))))
    (result (cons obj more-objects))))

(defun preposition (verb)
  (=let* ((prep (phrase-with-spaces)))
    (if (find prep (verb-prepositions verb) :test #'string-equal)
        (result prep)
        (fail))))

(defun transitive-verb-args (verb observer)
  (if (not (verb-transitive-p verb))
      (fail)
      (=let* ((_ (ws))
              (direct-objects (noun-clause observer))
              ;; Transitive verbs can have an adverb right in here.
              (adverb (maybe (=and (ws) (adverb)) 'error))
              (preposition (maybe (=prog2 (ws) (preposition verb) (ws))))
              (indirect-objects (if preposition
                                    (noun-clause observer)
                                    (result nil))))
        (result `((:adverb . ,(cdr adverb))
                  (:direct-objects . ,direct-objects)
                  (:preposition . ,preposition)
                  (:indirect-objects . ,indirect-objects))))))

(defun ditransitive-verb-args (verb observer)
  (if (not (verb-ditransitive-p verb))
      (fail)
      (=or
       (=let* ((_ (ws))
               (direct-objects (noun-clause observer))
               (preposition (=prog2 (ws) (=string "to") (ws)))
               (indirect-objects (noun-clause observer)))
         (result `((:direct-objects . ,direct-objects)
                   (:indirect-objects . ,indirect-objects)
                   (:preposition . ,preposition))))
       (=let* ((_ (ws))
               (indirect-objects (noun-clause observer))
               (_ (ws))
               (direct-objects (noun-clause observer)))
         (result `((:direct-objects . ,direct-objects)
                   (:indirect-objects . ,indirect-objects)))))))

(defun intransitive-verb-args (verb observer)
  (if (not (verb-intransitive-p verb))
      (fail)
      (=let* ((preposition (maybe (=prog2 (ws) (preposition verb) (ws))))
              (indirect-objects (if preposition
                                    (noun-clause observer)
                                    (result nil))))
        (result `((:indirect-objects . ,indirect-objects)
                  (:preposition . ,preposition))))))

(defun verb-args (verb observer)
  (=or (transitive-verb-args verb observer)
       (ditransitive-verb-args verb observer)
       (intransitive-verb-args verb observer)))

;; sentence = [adverb ws] verb [ws adverb] [ws verb-args] [ws adverb]
(defun sentence (observer)
  (=let* ((adverb1 (maybe (=prog1 (adverb) (ws)) 'error))
          (verb (verb))
          (adverb2 (maybe (=and (ws) (adverb)) 'error))
          (verb-args (verb-args verb observer))
          (adverb3 (maybe (=and (ws) (adverb)) 'error))
          (_ (maybe (=and (ws) (=char #\.) (ws)))))
    (let* ((all-adverbs (list (cdr adverb1)
                              (cdr adverb2)
                              (cdr (assoc :adverb verb-args))
                              (cdr adverb3)))
           (adverb-position (position-if-not #'null all-adverbs))
           (adverb (when adverb-position (elt all-adverbs adverb-position))))
      (cond ((< 1 (count-if #'stringp all-adverbs))
             (error "Too many adverbs. There can at most be one adverb."))
            (t
             (result `(:sentence
                       ,@(remove :adverb verb-args :key #'car)
                       (:adverb . ,adverb)
                       (:adverb-position . ,adverb-position)
                       (:verb . ,verb))))))))

;;; Commands
(defun parse-action (actor message)
  (let ((results (invoke-parser (either (=prog1 (sentence actor) (no-more-input)) 'error) message)))
    (cond ((null results)
           (error "ENOPARSE"))
          ((cdr results)
           (error "Parse was ambiguous."))
          ((typep (car results) 'error)
           (error (car results)))
          (t (let ((sentence (cdar results)))
               (apply #'invoke-verb-command :actor actor (alist-plist sentence)))))))

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
           (error (car results)))
          (t (values (cdr (assoc :dialogue (cdar results)))
                     (cdr (assoc :parenthetical (cdar results))))))))

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
