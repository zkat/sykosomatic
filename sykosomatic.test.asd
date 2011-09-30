;;;; sykosomatic.test.asd

(asdf:defsystem #:sykosomatic.test
  :serial t
  :depends-on (#:sykosomatic #:eos)
  :components
  ((:module test
            :serial t
            :components
            ((:file "test")
             (:file "command")))))
