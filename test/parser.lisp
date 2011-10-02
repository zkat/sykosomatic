(test:def-test-package parser
  (:use :sykosomatic.parser
        :sykosomatic.entity
        :sykosomatic.vocabulary
        :sykosomatic.command
        :sykosomatic.db
        :sykosomatic.game-objects.nameable))

(defmacro with-verb-and-nameables ((result-var verb-third-person name1 name2 &key
                                    transitivep intransitivep ditransitivep)
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
                        :ditransitivep ,ditransitivep)
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

(test action-parse
  (with-verb-and-nameables (results "testsmiles" "one" "two" :intransitivep t)
    (parse-action 1 "testsmiles")
    (is (equalp '(:actor 1 :verb "testsmiles" :adverbs (nil nil)
                  :do nil :io nil :preposition nil)
                results))
    (parse-action 1 "testsmiles at one")
    (is (equalp '(:actor 1 :verb "testsmiles" :adverbs (nil nil)
                  :do nil :io ("one") :preposition "at")
                results))))
