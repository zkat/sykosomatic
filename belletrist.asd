;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot)
  :components
  ((:file "package")
   (:file "belletrist")))

