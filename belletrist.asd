;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:yaclml #:clws #:bordeaux-threads)
  :components
  ((:file "package")
   (:file "belletrist")))

