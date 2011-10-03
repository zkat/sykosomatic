(test:def-test-package parser
  (:use :sykosomatic.parser
        :sykosomatic.entity
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.db
        :sykosomatic.game-objects.nameable))

(defmacro with-verb-and-nameables ((result-var verb-third-person name1 name2 &key
                                    transitivep intransitivep ditransitivep
                                    prepositions adverbs)
                                   &body body)
  (let ((verb-var (gensym "VERB-"))
        (adverbs-var (gensym "ADVERBS-")))
    `(let ((e1 (create-entity))
           (e2 (create-entity))
           (,verb-var ,verb-third-person)
           (,adverbs-var ,adverbs))
       (unwind-protect
            (let (,result-var)
              (add-name e1 ,name1)
              (add-name e2 ,name2)
              (map nil #'add-adverb ,adverbs-var)
              (add-verb ,verb-var :third-person ,verb-var
                        :transitivep ,transitivep
                        :intransitivep ,intransitivep
                        :ditransitivep ,ditransitivep
                        :prepositions ,prepositions)
              (defcommand test ()
                (setf results
                      (list :actor *actor*
                            :verb *verb*
                            :adverb *adverb*
                            :adverb-position *adverb-position*
                            :do (mapcar #'full-name *direct-objects*)
                            :io (mapcar #'full-name *indirect-objects*)
                            :preposition *preposition*)))
              (add-verb-command ,verb-var 'test)
              ,@body)
         (remove-command 'test)
         (remove-verb ,verb-var)
         (map nil #'remove-adverb ,adverbs-var)
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
    (is (string-equal "testsmiles" (verb-third-person (getf results :verb))))
    (finishes (parse-action 1 "testsmiles at one"))
    (is (string-equal "testsmiles" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (string-equal "at" (getf results :preposition)))
    (signals error (parse-action 1 "testsmiles one two"))
    (signals error (parse-action 1 "testsmiles one"))
    (signals error (parse-action 1 "testsmiles at one with two"))))

(test action-parse-transitive-intransitive
  (with-verb-and-nameables (results "testwaves" "one" "two"
                                    :intransitivep t :transitivep t
                                    :prepositions '("at" "to" "with"))
    (finishes (parse-action 1 "testwaves"))
    (is (string-equal "testwaves" (verb-third-person (getf results :verb))))
    (finishes (parse-action 1 "testwaves at one"))
    (is (string-equal "testwaves" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (string-equal "at" (getf results :preposition)))
    (finishes (parse-action 1 "testwaves one"))
    (is (string-equal "testwaves" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (null (getf results :preposition)))
    (finishes (parse-action 1 "testwaves one to two"))
    (is (string-equal "testwaves" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "to" (getf results :preposition)))
    (signals error (parse-action 1 "testwaves one two"))
    (signals error (parse-action 1 "testwaves at one with two"))))

(test action-parse-transitive
  (with-verb-and-nameables (results "testpunches" "one" "two" :transitivep t
                                    :prepositions '("at" "with"))
    (signals error (parse-action 1 "testpunches"))
    (signals error (parse-action 1 "testpunches at one"))
    (finishes (parse-action 1 "testpunches one"))
    (is (string-equal "testpunches" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (finishes (parse-action 1 "testpunches one with two"))
    (is (string-equal "testpunches" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "with" (getf results :preposition)))
    (signals error (parse-action 1 "testpunches one two"))
    (signals error (parse-action 1 "testpunches at one with two"))))

(test action-parse-ditransitive
  (with-verb-and-nameables (results "testgives" "one" "two" :ditransitivep t
                                    :prepositions '("at" "to" "with"))
    (signals error (parse-action 1 "testgives"))
    (signals error (parse-action 1 "testgives at one"))
    ;; ditransitive verbs only do 'to'
    (signals error (parse-action 1 "testgives two at one"))
    (signals error (parse-action 1 "testgives one"))
    (finishes (parse-action 1 "testgives one to two"))
    (is (string-equal "testgives" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "to" (getf results :preposition)))
    (finishes (parse-action 1 "testgives one two"))
    (is (string-equal "testgives" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (equalp '("two") (getf results :do)))
    (signals error (parse-action 1 "testgives at one with two"))))

(test action-parse-prepositions
  (with-verb-and-nameables (results "testsmiles" "one" "two" :intransitivep t
                                    :prepositions '("at" "with"))
    (finishes (parse-action 1 "testsmiles at one"))
    (is (string-equal "at" (getf results :preposition)))
    (finishes (parse-action 1 "testsmiles with one"))
    (is (string-equal "with" (getf results :preposition)))
    (signals error (parse-action 1 "testsmiles to one")))
  ;; Two-word
  (with-verb-and-nameables (results "teststands" "one" "two" :intransitivep t
                                    :prepositions '("right of" "next to"))
    (finishes (parse-action 1 "teststands right of one"))
    (is (string-equal "right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststands next to one"))
    (is (string-equal "next to" (getf results :preposition)))
    (signals error (parse-action 1 "teststands right one"))
    (signals error (parse-action 1 "teststands to one")))
  (with-verb-and-nameables (results "teststands" "one" "two" :intransitivep t
                                    :prepositions '("on" "on top of" "right of"
                                                    "to the right of" "next to"))
    (finishes (parse-action 1 "teststands to the right of one"))
    (is (string-equal "to the right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststands next to one"))
    (is (string-equal "next to" (getf results :preposition)))
    (finishes (parse-action 1 "teststands right of one"))
    (is (string-equal "right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststands on one"))
    (is (string-equal "on" (getf results :preposition)))
    (finishes (parse-action 1 "teststands on top of one"))
    (is (string-equal "on top of" (getf results :preposition)))
    (signals error (parse-action 1 "teststands right one"))
    (signals error (parse-action 1 "teststands to one"))))

(test action-parse-adverbs
  ;; Basic test
  (with-verb-and-nameables (results "teststands" "one" "two"
                                    :intransitivep t
                                    :prepositions '("on")
                                    :adverbs '("testly"))
    (finishes (parse-action 1 "teststands testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "teststands testly on one"))
    (is (string-equal "testly" (getf results :adverb))))
  ;; All adverb positions, including between do and io
  (with-verb-and-nameables (results "testsits" "one" "two"
                                    :intransitivep t
                                    :transitivep t
                                    :prepositions '("on")
                                    :adverbs '("testly"))
    (finishes (parse-action 1 "testly testsits"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 0 (getf results :adverb-position)))
    (finishes (parse-action 1 "testsits testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 1 (getf results :adverb-position)))
    (finishes (parse-action 1 "testsits testly one on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsits one on two testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsits one testly on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 2 (getf results :adverb-position)))
    (finishes (parse-action 1 "testly testsits one on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsits one on two testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 3 (getf results :adverb-position)))
    ;; Only one adverb allowed.
    (signals error (parse-action 1 "testly testsits testly one testly on two testly"))
    (signals error (parse-action 1 "testly testsits testly one testly on two"))
    (signals error (parse-action 1 "testly testsits testly one on two"))
    (signals error (parse-action 1 "testsits testly one testly on two testly"))
    (signals error (parse-action 1 "testsits one testly on two testly"))))

(test action-parse-multiple
  (with-verb-and-nameables (results "testpokes" "one" "two"
                                    :intransitivep t
                                    :transitivep t
                                    :prepositions '("in" "at"))
    (finishes (parse-action 1 "testpokes at one, two"))
    (is (equalp '("one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpokes one, two, one"))
    (is (equalp '("one" "two" "one") (getf results :do)))
    (finishes (parse-action 1 "testpokes at one and two"))
    (is (equalp '("one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpokes at one, one and two"))
    (is (equalp '("one" "one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpokes at one, one, and two"))
    (is (equalp '("one" "one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpokes one, two in two, one"))
    (is (equalp '("one" "two") (getf results :do)))
    (is (equalp '("two" "one") (getf results :io)))))
