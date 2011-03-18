;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:yaclml #:clws #:bordeaux-threads #:jsown)
  :components
  ((:file "package")
   (:file "belletrist")))

