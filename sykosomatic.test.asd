;;;; sykosomatic.test.asd

(asdf:defsystem #:sykosomatic.test
  :serial t
  :depends-on (#:sykosomatic #:eos #:drakma)
  :components
  ((:module test
            :serial t
            :components
            ((:file "test")
             (:module util
                      :components
                      ((:file "form")))
             (:file "entity")
             (:module components
                      :components
                      ((:file "nameable")
                       (:file "container")))
             (:file "session")
             (:file "command")
             (:file "parser")))))
