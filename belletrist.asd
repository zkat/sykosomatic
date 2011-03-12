;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:yaclml)
  :components
  ((:file "package")
   (:file "belletrist")))

