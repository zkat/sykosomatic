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
            ((:file "utils")
             (:file "db")
             (:file "smug")
             (:file "account")
             (:file "character")
             (:file "scene")
             (:file "sykosomatic")
             (:file "websocket")
             (:file "parser")
             (:file "handlers")
             (:file "main")))))
