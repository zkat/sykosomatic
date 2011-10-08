(test:def-test-package parser
  (:use :sykosomatic.parser
        :sykosomatic.entity
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.db
        :sykosomatic.components.describable))

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
              (configure-noun e1 ,name1 :use-article-p nil)
              (configure-noun e2 ,name2 :use-article-p nil)
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
                            :do (mapcar (curry #'short-description *actor*) *direct-objects*)
                            :io (mapcar (curry #'short-description *actor*) *indirect-objects*)
                            :preposition *preposition*)))
              (add-verb-command ,verb-var 'test)
              ,@body)
         (remove-command 'test)
         (remove-verb-command ,verb-var)
         (remove-verb ,verb-var)
         (map nil #'remove-adverb ,adverbs-var)
         (remove-noun e1)
         (remove-noun e2)
         (delete-entities e1 e2)))))

(test action-parse-intransitive
  (with-verb-and-nameables (results "testsmile" "one" "two" :intransitivep t
                                    :prepositions '("at" "with"))
    (finishes (parse-action 1 "testsmile"))
    (is (string-equal "testsmile" (verb-third-person (getf results :verb))))
    (finishes (parse-action 1 "testsmile at one"))
    (is (string-equal "testsmile" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (string-equal "at" (getf results :preposition)))
    (signals error (parse-action 1 "testsmile one two"))
    (signals error (parse-action 1 "testsmile one"))
    (signals error (parse-action 1 "testsmile at one with two"))))

(test action-parse-transitive-intransitive
  (with-verb-and-nameables (results "testwave" "one" "two"
                                    :intransitivep t :transitivep t
                                    :prepositions '("at" "to" "with"))
    (finishes (parse-action 1 "testwave"))
    (is (string-equal "testwave" (verb-third-person (getf results :verb))))
    (finishes (parse-action 1 "testwave at one"))
    (is (string-equal "testwave" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (string-equal "at" (getf results :preposition)))
    (finishes (parse-action 1 "testwave one"))
    (is (string-equal "testwave" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (null (getf results :preposition)))
    (finishes (parse-action 1 "testwave one to two"))
    (is (string-equal "testwave" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "to" (getf results :preposition)))
    (signals error (parse-action 1 "testwave one two"))
    (signals error (parse-action 1 "testwave at one with two"))))

(test action-parse-transitive
  (with-verb-and-nameables (results "testpunch" "one" "two" :transitivep t
                                    :prepositions '("at" "with"))
    (signals error (parse-action 1 "testpunch"))
    (signals error (parse-action 1 "testpunch at one"))
    (finishes (parse-action 1 "testpunch one"))
    (is (string-equal "testpunch" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (finishes (parse-action 1 "testpunch one with two"))
    (is (string-equal "testpunch" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "with" (getf results :preposition)))
    (signals error (parse-action 1 "testpunch one two"))
    (signals error (parse-action 1 "testpunch at one with two"))))

(test action-parse-ditransitive
  (with-verb-and-nameables (results "testgive" "one" "two" :ditransitivep t
                                    :prepositions '("at" "to" "with"))
    (signals error (parse-action 1 "testgive"))
    (signals error (parse-action 1 "testgive at one"))
    ;; ditransitive verbs only do 'to'
    (signals error (parse-action 1 "testgive two at one"))
    (signals error (parse-action 1 "testgive one"))
    (finishes (parse-action 1 "testgive one to two"))
    (is (string-equal "testgive" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :do)))
    (is (equalp '("two") (getf results :io)))
    (is (string-equal "to" (getf results :preposition)))
    (finishes (parse-action 1 "testgive one two"))
    (is (string-equal "testgive" (verb-third-person (getf results :verb))))
    (is (equalp '("one") (getf results :io)))
    (is (equalp '("two") (getf results :do)))
    (signals error (parse-action 1 "testgive at one with two"))))

(test action-parse-prepositions
  (with-verb-and-nameables (results "testsmile" "one" "two" :intransitivep t
                                    :prepositions '("at" "with"))
    (finishes (parse-action 1 "testsmile at one"))
    (is (string-equal "at" (getf results :preposition)))
    (finishes (parse-action 1 "testsmile with one"))
    (is (string-equal "with" (getf results :preposition)))
    (signals error (parse-action 1 "testsmile to one")))
  ;; Two-word
  (with-verb-and-nameables (results "teststand" "one" "two" :intransitivep t
                                    :prepositions '("right of" "next to"))
    (finishes (parse-action 1 "teststand right of one"))
    (is (string-equal "right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststand next to one"))
    (is (string-equal "next to" (getf results :preposition)))
    (signals error (parse-action 1 "teststand right one"))
    (signals error (parse-action 1 "teststand to one")))
  (with-verb-and-nameables (results "teststand" "one" "two" :intransitivep t
                                    :prepositions '("on" "on top of" "right of"
                                                    "to the right of" "next to"))
    (finishes (parse-action 1 "teststand to the right of one"))
    (is (string-equal "to the right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststand next to one"))
    (is (string-equal "next to" (getf results :preposition)))
    (finishes (parse-action 1 "teststand right of one"))
    (is (string-equal "right of" (getf results :preposition)))
    (finishes (parse-action 1 "teststand on one"))
    (is (string-equal "on" (getf results :preposition)))
    (finishes (parse-action 1 "teststand on top of one"))
    (is (string-equal "on top of" (getf results :preposition)))
    (signals error (parse-action 1 "teststand right one"))
    (signals error (parse-action 1 "teststand to one"))))

(test action-parse-adverbs
  ;; Basic test
  (with-verb-and-nameables (results "teststand" "one" "two"
                                    :intransitivep t
                                    :prepositions '("on")
                                    :adverbs '("testly"))
    (finishes (parse-action 1 "teststand testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "teststand testly on one"))
    (is (string-equal "testly" (getf results :adverb))))
  ;; All adverb positions, including between do and io
  (with-verb-and-nameables (results "testsit" "one" "two"
                                    :intransitivep t
                                    :transitivep t
                                    :prepositions '("on")
                                    :adverbs '("testly"))
    (finishes (parse-action 1 "testly testsit"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 0 (getf results :adverb-position)))
    (finishes (parse-action 1 "testsit testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 1 (getf results :adverb-position)))
    (finishes (parse-action 1 "testsit testly one on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsit one on two testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsit one testly on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 2 (getf results :adverb-position)))
    (finishes (parse-action 1 "testly testsit one on two"))
    (is (string-equal "testly" (getf results :adverb)))
    (finishes (parse-action 1 "testsit one on two testly"))
    (is (string-equal "testly" (getf results :adverb)))
    (is (= 3 (getf results :adverb-position)))
    ;; Only one adverb allowed.
    (signals error (parse-action 1 "testly testsit testly one testly on two testly"))
    (signals error (parse-action 1 "testly testsit testly one testly on two"))
    (signals error (parse-action 1 "testly testsit testly one on two"))
    (signals error (parse-action 1 "testsit testly one testly on two testly"))
    (signals error (parse-action 1 "testsit one testly on two testly"))))

(test action-parse-multiple
  (with-verb-and-nameables (results "testpoke" "one" "two"
                                    :intransitivep t
                                    :transitivep t
                                    :prepositions '("in" "at"))
    (finishes (parse-action 1 "testpoke at one, two"))
    (is (equalp '("one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpoke one, two, one"))
    (is (equalp '("one" "two" "one") (getf results :do)))
    (finishes (parse-action 1 "testpoke at one and two"))
    (is (equalp '("one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpoke at one, one and two"))
    (is (equalp '("one" "one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpoke at one, one, and two"))
    (is (equalp '("one" "one" "two") (getf results :io)))
    (finishes (parse-action 1 "testpoke one, two in two, one"))
    (is (equalp '("one" "two") (getf results :do)))
    (is (equalp '("two" "one") (getf results :io)))))
