;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:yaclml #:ht-simple-ajax)
  :components
  ((:file "package")
   (:file "belletrist")))

