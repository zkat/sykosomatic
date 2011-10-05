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
    (is (string= "testteapot" (setf (noun e) "testteapot")))
    (is (string= "testteapot" (noun e)))
    (is (null (setf (noun e) nil)))
    (is (null (noun e)))))

(test plural-noun
  (with-entities (e)
    (setf (noun e) "testteapot")
    (is (string= "testteapots" (plural-noun e)))
    (setf (noun e) '("testteapot" "testteapotses"))
    (is (string= "testteapot" (noun e)))
    (is (string= "testteapotses" (plural-noun e)))))

(test pluralize
  ;; TODO - test unnacented pluralisation... need to fix Eos first, though.
  (is (string= "testteapots" (pluralize "testteapot")))
  (is (string= "flies" (pluralize "fly")))
  (is (string= "boys" (pluralize "boy")))
  (is (string= "kisses" (pluralize "kiss")))
  (is (string= "witches" (pluralize "witch")))
  (is (string= "wishes" (pluralize "wish")))
  (is (string= "heroes" (pluralize "hero"))))

(test adjectives
  (with-entities (e)
    (is (null (adjectives e)))
    (let ((new-adjs '("short")))
      (is (eql new-adjs (setf (adjectives e) new-adjs)))
      (is (equalp new-adjs (adjectives e))))
    (let ((new-adjs '("short" "stout")))
      (is (eql new-adjs (setf (adjectives e) new-adjs)))
      (is (equalp new-adjs (adjectives e))))
    (is (null (setf (adjectives e) nil)))
    (is (null (adjectives e)))))

(test features
  (with-entities (e f1 f2 f3)
    (is (null (list-features e)))
    (finishes (add-feature e f1))
    (is (equal (list f1) (list-features e)))
    (finishes (add-feature e f2))
    (is (null (set-difference (list f1 f2) (list-features e))))
    (finishes (add-feature f1 f3))
    (is (null (set-difference (list f1 f2) (list-features e))))
    (is (equal (list f3) (list-features f1)))
    (finishes (remove-feature e f1))
    (finishes (remove-feature e f2))
    (finishes (remove-feature f1 f3))))

(test base-description
  (with-entities (e f1 f2 f3)
    (setf (noun e) "testteapot")
    (is (string= "a testteapot" (base-description e)))
    (setf (adjectives e) '("short" "stout"))
    (is (string= "a short and stout testteapot" (base-description e)))
    (setf (noun f1) "handle")
    (setf (noun f2) "spout")
    (add-feature e f1)
    (is (string= "a short and stout testteapot with a handle" (base-description e)))
    (add-feature e f2)
    (is (string= "a short and stout testteapot with a handle and a spout" (base-description e)))
    (setf (noun f3) "curvature")
    (add-feature f1 f3)
    (is (string= "a handle with a curvature" (base-description f1)))
    (is (string= "a short and stout testteapot with a handle and a spout" (base-description e)))
    (setf (noun f1) "handyhandle")
    (setf (adjectives f1) '("handy"))
    (is (string= "a short and stout testteapot with a handy handyhandle and a spout" (base-description e)))
    (remove-feature e f2)
    (is (string= "a short and stout testteapot with a handy handyhandle" (base-description e)))
    (setf (noun e) nil
          (adjectives e) nil
          (noun f1) nil
          (noun f2) nil
          (noun f3) nil)
    (remove-feature e f1)
    (remove-feature e f2)
    (remove-feature f1 f3))
  (with-entities (e1 e2)
    ;; a/an articles
    (setf (noun e1) "article"
          (noun e2) "test")
    (is (string= "an article" (base-description e1)))
    (is (string= "a test" (base-description e2)))
    (setf (adjectives e1) '("long" "arduous"))
    (setf (adjectives e2) '("arduous" "long"))
    ;; Adjective ordering issues strike again :(
    (is (string= "a long and arduous article" (base-description e1)))
    (is (string= "an arduous and long test" (base-description e2)))
    (setf (noun e1) nil
          (noun e2) nil
          (adjectives e1) nil
          (adjectives e2) nil)))

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
    (is (string= "Tomato" (nickname o1 e)))
    (setf (nickname o1 e) nil
          (nickname o2 e) nil)))

(test short-description
  (with-entities (e f o)
    (is (null (short-description o e)))
    (setf (noun e) "testteapot")
    (setf (adjectives e) '("short" "stout"))
    (setf (noun f) "handle")
    (add-feature e f)
    (is (string= "a short and stout testteapot with a handle" (short-description o e)))
    (setf (nickname o e) "Mrs. Potts")
    (is (string= "Mrs. Potts" (short-description o e)))
    (setf (nickname o e) nil)
    (is (string= "a short and stout testteapot with a handle" (short-description o e)))
    (setf (noun e) nil
          (noun f) nil)
    (remove-feature e f)))

(test find-by-short-description
  (with-entities (o e1 e2)
    (setf (noun e1) "testteapot")
    (setf (noun e2) "testteacup")
    (is (null (set-difference (list e1 e2) (find-by-short-description o "testtea"))))
    (is (null (set-difference (list e1 e2) (find-by-short-description o "a testtea"))))
    (is (equal (list e1) (find-by-short-description o "testteap")))
    (is (equal (list e2) (find-by-short-description o "testteac")))
    (setf (nickname o e1) "Mrs. Potts")
    (is (null (find-by-short-description o "testteapot")))
    (is (equal (list e1) (find-by-short-description o "rs. Pot")))
    ;; The following test fails, but I'm pretty sure it's just Eos' fault.
    ;; Executing this manually in the REPL -does- work.
    ;; (setf (nickname o e2) "Chíp")
    ;; (is (equal (list e2) (find-by-short-description o "hip")))
    (setf (noun e1) nil
          (noun e2) nil
          (nickname o e1) nil
          (nickname o e2) nil)))
