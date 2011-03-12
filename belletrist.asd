;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on '(alexandria)
  :components
  ((:file "package")
   (:file "belletrist")))

