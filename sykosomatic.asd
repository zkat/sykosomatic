;;;; sykosomatic.asd

(asdf:defsystem #:sykosomatic
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot #:yaclml #:clws
               #:bordeaux-threads #:cl-ppcre
               #:jsown #:ironclad #:postmodern
               #:string-case)
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "smug")
             (:file "db")
             (:file "entity")
             (:file "account")
             (:file "character")
             (:file "scene")
             (:file "sykosomatic")
             (:file "websocket")
             (:file "parser")
             (:file "templates")
             (:file "newchar-templates")
             (:file "handlers")
             (:file "main")))))
