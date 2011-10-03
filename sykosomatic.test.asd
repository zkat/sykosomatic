;;;; sykosomatic.test.asd

(asdf:defsystem #:sykosomatic.test
  :serial t
  :depends-on (#:sykosomatic #:eos)
  :components
  ((:module test
            :serial t
            :components
            ((:file "test")
             (:module components
                      :components
                      ((:file "nameable")))
             (:file "command")
             (:file "parser")))))
