;;;; sykosomatic.asd

(asdf:defsystem #:sykosomatic
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot #:yaclml #:clws
               #:bordeaux-threads #:cl-ppcre
               #:jsown #:ironclad
               #:chillax.core #:chillax.jsown)
  :components
  ((:module src
            :serial t
            :components
            ((:file "db")
             (:file "smug")
             (:file "parser")
             (:file "account")
             (:file "character")
             (:file "scene")
             (:file "sykosomatic")
             (:file "handlers")
             (:file "main")))))
