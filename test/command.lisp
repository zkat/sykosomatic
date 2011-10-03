(test:def-test-package command
  (:use :sykosomatic.command
        :sykosomatic.vocabulary))

(test command-basic
  (unwind-protect
       (let ((success (gensym)))
         (defcommand test ()
           (list *actor* *verb* *adverb* *adverb-position*
                 *direct-objects* *indirect-objects* *preposition*)
           success)
         (add-verb-command "smile" 'test)
         (is (eq success (invoke-verb-command :verb (find-verb "smile")))))
    (remove-verb-command "smile")
    (remove-command 'test)))
