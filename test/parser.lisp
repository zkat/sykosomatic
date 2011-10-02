(test:def-test-package parser
  (:use :sykosomatic.parser
        :sykosomatic.entity
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.db
        :sykosomatic.game-objects.nameable))

(defmacro with-verb-and-nameables ((result-var verb-third-person name1 name2 &key
                                    transitivep intransitivep ditransitivep
                                    prepositions)
                                   &body body)
  (let ((verb-var (gensym "VERB-")))
    `(let ((e1 (create-entity))
           (e2 (create-entity))
           (,verb-var ,verb-third-person))
       (unwind-protect
            (let (,result-var)
              (add-name e1 ,name1)
              (add-name e2 ,name2)
              (add-verb ,verb-var :third-person ,verb-var
                        :transitivep ,transitivep
                        :intransitivep ,intransitivep
                        :ditransitivep ,ditransitivep
                        :prepositions ,prepositions)
              (defcommand test ()
                (setf results
                      (list :actor *actor*
                            :verb *verb*
                            :adverbs *adverbs*
                            :do (mapcar #'full-name *direct-objects*)
                            :io (mapcar #'full-name *indirect-objects*)
                            :preposition *preposition*)))
              (add-verb-command ,verb-var 'test)
              ,@body)
         (remove-command 'test)
         (remove-verb ,verb-var)
         (db-query (:delete-from 'nameable :where (:or
                                                   (:= 'entity-id e1)
                                                   (:= 'entity-id e2))))
         (db-query (:delete-from 'entity :where (:or
                                                 (:= 'id e1)
                                                 (:= 'id e2))))))))

(test action-parse-intransitive
  (with-verb-and-nameables (results "testsmiles" "one" "two" :intransitivep t
                                    :prepositions '("at" "with"))
    (finishes (parse-action 1 "testsmiles"))
    (is (equalp '(:actor 1 :verb "testsmiles" :adverbs (nil nil)
                  :do nil :io nil :preposition nil)
                results))
    (finishes (parse-action 1 "testsmiles at one"))
    (is (equalp '(:actor 1 :verb "testsmiles" :adverbs (nil nil)
                  :do nil :io ("one") :preposition "at")
                results))
    (signals error (parse-action 1 "testsmiles one two"))
    (signals error (parse-action 1 "testsmiles one"))
    (signals error (parse-action 1 "testsmiles at one with two"))))

(test action-parse-transitive-intransitive
  (with-verb-and-nameables (results "testwaves" "one" "two"
                                    :intransitivep t :transitivep t
                                    :prepositions '("at" "to" "with"))
    (finishes (parse-action 1 "testwaves"))
    (is (equalp '(:actor 1 :verb "testwaves" :adverbs (nil nil)
                  :do nil :io nil :preposition nil)
                results))
    (finishes (parse-action 1 "testwaves at one"))
    (is (equalp '(:actor 1 :verb "testwaves" :adverbs (nil nil)
                  :do nil :io ("one") :preposition "at")
                results))
    (finishes (parse-action 1 "testwaves one"))
    (is (equalp '(:actor 1 :verb "testwaves" :adverbs (nil nil)
                  :do ("one") :io nil :preposition nil)
                results))
    (finishes (parse-action 1 "testwaves one to two"))
    (is (equalp '(:actor 1 :verb "testwaves" :adverbs (nil nil)
                  :do ("one") :io ("two") :preposition "to")
                results))
    (signals error (parse-action 1 "testwaves one two"))
    (signals error (parse-action 1 "testwaves at one with two"))))

(test action-parse-transitive
  (with-verb-and-nameables (results "testpunches" "one" "two" :transitivep t
                                    :prepositions '("at" "with"))
    (signals error (parse-action 1 "testpunches"))
    (signals error (parse-action 1 "testpunches at one"))
    (finishes (parse-action 1 "testpunches one"))
    (is (equalp '(:actor 1 :verb "testpunches" :adverbs (nil nil)
                  :do ("one") :io nil :preposition nil)
                results))
    (finishes (parse-action 1 "testpunches one with two"))
    (is (equalp '(:actor 1 :verb "testpunches" :adverbs (nil nil)
                  :do ("one") :io ("two") :preposition "with")
                results))
    (signals error (parse-action 1 "testpunches one two"))
    (signals error (parse-action 1 "testpunches at one with two"))))

(test action-parse-ditransitive
  (with-verb-and-nameables (results "testgives" "one" "two" :ditransitivep t
                                    :prepositions '("at" "to" "with"))
    (signals error (parse-action 1 "testgives"))
    (signals error (parse-action 1 "testgives at one"))
    (signals error (parse-action 1 "testgives one"))
    (finishes (parse-action 1 "testgives one to two"))
    (is (equalp '(:actor 1 :verb "testgives" :adverbs (nil nil)
                  :do ("one") :io ("two") :preposition "to")
                results))
    (finishes (parse-action 1 "testgives one two"))
    (is (equalp '(:actor 1 :verb "testgives" :adverbs (nil nil)
                  :do ("two") :io ("one") :preposition nil)
                results))
    (signals error (parse-action 1 "testgives at one with two"))))
