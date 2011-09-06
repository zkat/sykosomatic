;;;; sykosomatic.asd

(asdf:defsystem #:sykosomatic
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot #:yaclml #:clws
               #:bordeaux-threads #:cl-ppcre
               #:jsown #:ironclad #:postmodern
               #:local-time
               #:string-case)
  :components
  ((:module src
            :serial t
            :components
            ((:module util
                      :serial t
                      :components
                      ((:file "utils")
                       (:file "smug")
                       (:file "queue")
                       (:file "timer")))
             (:file "db")
             (:file "entity")
             (:file "account")
             (:module game-objects
                      :components
                      ((:file "describable")))
             (:file "character")
             (:file "scene")
             (:file "sykosomatic")
             (:file "sessions")
             (:file "websocket")
             (:file "parser")
             (:file "templates")
             (:file "newchar-templates")
             (:file "handlers")
             (:file "main")))))
