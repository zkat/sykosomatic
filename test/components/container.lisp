(test:def-test-package components.container
  (:use :sykosomatic.db
        :sykosomatic.entity
        :sykosomatic.components.container))

(defmacro with-entities ((&rest entity-vars) &body body)
  `(let ,(mapcar (rcurry #'list '(create-entity))
                 entity-vars)
     (unwind-protect (progn ,@body)
       (db-query (:delete-from 'entity
                               :where (:in 'id (:set (list ,@entity-vars))))))))

(test container-contents
  (with-entities (e1 e2 container)
    (move-to-container e1 container)
    (is (null (set-difference (list e1) (container-contents container))))
    (move-to-container e2 container)
    (is (null (set-difference (list e1 e2) (container-contents container))))))

(test entity-container
  (with-entities (e container1 container2)
    (is (null (entity-container e)))
    (move-to-container e container1)
    (is (= container1 (entity-container e)))
    (move-to-container e container2)
    (is (= container2 (entity-container e)))))

(test remove-from-container
  (with-entities (e container)
    (finishes (remove-from-container e))
    (move-to-container e container)
    (is (equal (list e) (container-contents container)))
    (finishes (remove-from-container e))
    (is (null (container-contents container)))))

(test move-to-container
  (with-entities (e container1 container2)
    (finishes (move-to-container e container1))
    (is (equal (list e) (container-contents container1)))
    (is (null (container-contents container2)))
    (finishes (move-to-container e container2))
    (is (equal (list e) (container-contents container2)))
    (is (null (container-contents container1)))
    (finishes (move-to-container e container2))
    (is (equal (list e) (container-contents container2)))
    (is (null (container-contents container1)))))

(test others-in-container
  (with-entities (e1 e2 e3 container)
    (move-to-container e1 container)
    (is (null (others-in-container e1)))
    (move-to-container e2 container)
    (is (equal (list e2) (others-in-container e1)))
    (is (equal (list e1) (others-in-container e2)))
    (move-to-container e3 container)
    (is (null (set-difference (list e1 e2) (others-in-container e3))))))

(test remove-from-container
  (with-entities (e container)
    (move-to-container e container)
    (finishes (remove-from-container e))
    (is (null (container-contents container)))))
