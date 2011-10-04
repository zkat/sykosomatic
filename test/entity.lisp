(test:def-test-package entity
  (:use :sykosomatic.db
        :sykosomatic.entity))

;; TODO - test comments
(test create-entity
  (let (e1 e2)
    (unwind-protect
         (progn
           (finishes (setf e1 (create-entity)))
           (finishes (setf e2 (create-entity)))
           (is (numberp e1))
           (is (not (eql e1 e2)))
           ;; IS seems to mess with DB-QUERY.
           (let ((entity-exists-p (db-query (:select t :from 'entity :where (:= 'id e1)) :single)))
             (is (not (null entity-exists-p)))))
      (delete-entities e1 e2))))

(test delete-entities
  (let ((e (create-entity)))
    (finishes (delete-entities e))
    (let ((entity-exists-p (db-query (:select t :from 'entity :where (:= 'id e)) :single)))
      (is (null entity-exists-p))))
  (let ((e1 (create-entity))
        (e2 (create-entity)))
    (finishes (delete-entities e1 e2))
    (let ((entities-exist-p (db-query (:select t :from 'entity :where (:or (:= 'id e1) (:= 'id e2)))
                                      :single)))
      (is (null entities-exist-p)))))

(test entity-oid)
(test find-entity-by-oid)
