(util:def-file-package #:sykosomatic.command.social
  (:use :sykosomatic.command
        :sykosomatic.vocabulary
        :sykosomatic.components.describable))

(defcommand social ()
  (do-local-entities (e *actor*)
    (tell e (with-output-to-string (s)
              (princ (short-description *actor* *actor*) s)
              (when (eql 0 *adverb-position*)
                (format s " ~A " *adverb*))
              (format s " ~A" (verb-third-person *verb*))
              (when (eql 1 *adverb-position*)
                (format s " ~A" *adverb*))
              (when (null *preposition*)
                (when *indirect-objects* (princ " " s))
                (format s *english-list-format-string* (mapcar #'short-description
                                                               *indirect-objects*)))
              (when *direct-objects* (princ " " s))
              (format s *english-list-format-string* (mapcar #'short-description
                                                             *direct-objects*))
              (when (eql 2 *adverb-position*)
                (format s " ~A" *adverb*))
              (when *preposition*
                (format s " ~A" *preposition*)
                (when *indirect-objects* (princ " " s))
                (format s *english-list-format-string* (mapcar #'short-description
                                                               *indirect-objects*)))
              (when (eql 3 *adverb-position*)
                (format s " ~A" *adverb*))
              (princ "." s)))))

;;; Testing
#+nil
(add-verb-command "smiles" 'social)
#+nil
(add-verb-command "punches" 'social)
