(test:def-test-package components.describable
  (:use :sykosomatic.entity
        :sykosomatic.components.describable))

(defmacro with-entities ((&rest entity-vars) &body body)
  `(let ,(mapcar (rcurry #'list '(create-entity))
                 entity-vars)
     (unwind-protect (progn ,@body)
       (delete-entities ,@entity-vars))))

(test noun
  (with-entities (e)
    (is (string= "teapot" (setf (noun e) "teapot")))
    (is (string= "teapot" (noun e)))
    (is (null (setf (noun e) nil)))
    (is (null (noun e)))))

(test adjectives
  (with-entities (e)
    (is (null (adjectives e)))
    (let ((new-adjs '("short")))
      (is (eql new-adjs (setf (adjectives e) new-adjs)))
      (is (equalp new-adjs (adjectives e))))
    (is (null (setf (adjectives e) nil)))
    (is (null (adjectives e)))))

(test base-description
  (with-entities (e)
    (setf (noun e) "teapot")
    (is (string= "a teapot" (base-description e)))
    (setf (adjectives e) '("short" "stout"))
    (is (string= "a short and stout teapot" (base-description e)))))

(test nickname
  (with-entities (e o)
    (is (null (nickname o e)))
    (is (string= "John Crichton" (setf (nickname o e) "John Crichton")))
    (is (string= "John Crichton" (nickname o e)))
    (is (null (setf (nickname o e) nil)))
    (is (null (nickname o e))))
  (with-entities (e o1 o2)
    (setf (nickname o1 e) "Tomato")
    (is (null (nickname o2 e)))
    (is (string= "Tomato" (nickname o1 e)))
    (setf (nickname o2 e) "Tomawtoe")
    (is (string= "Tomawtoe" (nickname o2 e)))
    (is (string= "Tomato" (nickname o1 e)))))

(test short-description
  (with-entities (e o)
    (is (null (short-description o e)))
    (setf (noun e) "teapot")
    (setf (adjectives e) '("short" "stout"))
    (is (string= "a short and stout teapot" (short-description o e)))
    (setf (nickname o e) "Mrs. Potts")
    (is (string= "Mrs. Potts" (short-description o e)))
    (setf (nickname o e) nil)
    (is (string= "a short and stout teapot" (short-description o e)))))
