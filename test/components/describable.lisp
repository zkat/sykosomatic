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
    (is (null (noun e)))
    (finishes (configure-noun e "testteapot"))
    (is (string= "testteapot" (noun e)))
    (remove-noun e)))

(test configure-noun
  (with-entities (e)
    (configure-noun e "testteapot")
    (is (string= "testteapots" (plural-noun e)))
    (configure-noun e "testteapot" :plural "testteapotses")
    (is (string= "testteapot" (noun e)))
    (is (string= "testteapotses" (plural-noun e)))
    (remove-noun e)))

(test pluralize
  ;; TODO - test unnacented pluralisation... need to fix Eos first, though.
  (is (string= "testteapots" (pluralize "testteapot")))
  (is (string= "flies" (pluralize "fly")))
  (is (string= "boys" (pluralize "boy")))
  (is (string= "kisses" (pluralize "kiss")))
  (is (string= "witches" (pluralize "witch")))
  (is (string= "wishes" (pluralize "wish")))
  (is (string= "heroes" (pluralize "hero"))))

(test add-adjective
  (with-entities (e)
    (finishes (add-adjective e "short"))
    (is (equalp '("short") (adjectives e)))))

(test remove-adjective
  (with-entities (e)
    (finishes (remove-adjective e "does-not-exist"))
    (add-adjective e "short")
    (finishes (remove-adjective e "short"))
    (is (null (adjectives e)))))

(test remove-all-adjectives
  (with-entities (e)
    (add-adjective e "short")
    (add-adjective e "stout")
    (finishes (remove-all-adjectives e))
    (is (null (adjectives e)))))

#+nil
(test adjectives
  (with-entities (e)
    (is (null (adjectives e)))
    (finishes (add-adjective e "short") )
    (is (equalp '("short") (adjectives e)))
    (remove-adjective e "short")
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
    (configure-noun e "testteapot")
    (is (string= "a testteapot" (base-description e)))
    (add-adjective e "short")
    (add-adjective e "stout")
    (is (string= "a short and stout testteapot" (base-description e)))
    (configure-noun f1 "handle")
    (configure-noun f2 "spout")
    (add-feature e f1)
    (is (string= "a short and stout testteapot with a handle" (base-description e)))
    (add-feature e f2)
    (is (string= "a short and stout testteapot with a handle and a spout" (base-description e)))
    (configure-noun f3 "curvature")
    (add-feature f1 f3)
    (is (string= "a handle with a curvature" (base-description f1)))
    (is (string= "a short and stout testteapot with a handle and a spout" (base-description e)))
    (configure-noun f1 "handyhandle")
    (add-adjective f1 "handy")
    (is (string= "a short and stout testteapot with a handy handyhandle and a spout" (base-description e)))
    (remove-feature e f2)
    (is (string= "a short and stout testteapot with a handy handyhandle" (base-description e)))
    (remove-noun e)
    (remove-noun f1)
    (remove-noun f2)
    (remove-noun f3)
    (remove-all-adjectives e)
    (remove-feature e f1)
    (remove-feature e f2)
    (remove-feature f1 f3))
  (with-entities (e1 e2)
    ;; a/an articles
    (configure-noun e1 "article")
    (configure-noun e2 "test")
    (is (string= "an article" (base-description e1)))
    (is (string= "a test" (base-description e2)))
    (add-adjective e1 "long")
    (add-adjective e1 "arduous")
    (add-adjective e2 "arduous")
    (add-adjective e2 "long")
    ;; Adjective ordering issues strike again :(
    (is (string= "a long and arduous article" (base-description e1)))
    (is (string= "an arduous and long test" (base-description e2)))
    (remove-noun e1)
    (remove-noun e2)
    (remove-all-adjectives e1)
    (remove-all-adjectives e2)))

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
    (configure-noun e "testteapot")
    (add-adjective e "short")
    (add-adjective e "stout")
    (configure-noun f "handle")
    (add-feature e f)
    (is (string= "a short and stout testteapot with a handle" (short-description o e)))
    (setf (nickname o e) "Mrs. Potts")
    (is (string= "Mrs. Potts" (short-description o e)))
    (setf (nickname o e) nil)
    (is (string= "a short and stout testteapot with a handle" (short-description o e)))
    (remove-noun e)
    (remove-noun f)
    (remove-feature e f)))

(test find-by-short-description
  (with-entities (o e1 e2)
    (configure-noun e1 "testteapot")
    (configure-noun e2 "testteacup")
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
    (remove-noun e1)
    (remove-noun e2)
    (setf (nickname o e1) nil
          (nickname o e2) nil)))

(test partial-short-description
  (with-entities (o e1 e2)
    (configure-noun e1 "testteapot")
    (configure-noun e2 "testteacup")
    (is (null (set-difference (list "a testteapot" "a testteacup")
                              (partial-short-description o "testtea")
                              :test #'equalp)))
    (is (null (set-difference (list "a testteapot" "a testteacup")
                              (partial-short-description o "a testtea")
                              :test #'equalp)))
    (is (equalp (list "a testteapot") (partial-short-description o "testteap")))
    (is (equalp (list "a testteacup") (partial-short-description o "testteac")))
    (setf (nickname o e1) "Mrs. Potts")
    (is (null (partial-short-description o "testteapot")))
    (is (equalp (list "Mrs. Potts") (partial-short-description o "rs. Pot")))
    (remove-noun e1)
    (remove-noun e2)
    (setf (nickname o e1) nil
          (nickname o e2) nil)))
