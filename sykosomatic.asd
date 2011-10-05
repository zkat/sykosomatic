;;;; sykosomatic.asd

(asdf:defsystem #:sykosomatic
  :serial t
  :depends-on (#:alexandria #:cl-speedy-queue
               #:hunchentoot #:yaclml #:clws
               #:bordeaux-threads #:cl-ppcre
               #:jsown #:ironclad #:postmodern
               #:local-time #:html-template
               #:string-case)
  :components
  ((:module src
            :serial t
            :components
            ((:module util
                      :serial t
                      :components
                      ((:file "util")
                       (:file "form")
                       (:file "smug")
                       (:file "timer")))
             (:file "config")
             (:file "template")
             (:file "db")
             (:file "vocabulary")
             (:file "entity")
             (:module components
                      :components
                      ((:file "describable")
                       (:file "nameable")
                       (:file "container")))
             (:file "account")
             (:file "character-creation")
             (:file "scene")
             (:file "session")
             (:file "command")
             (:file "parser")
             (:file "websocket")
             (:file "handler")
             (:module handlers
                      :components
                      ((:file "404")
                       (:file "index")
                       (:file "login")
                       (:file "logout")
                       (:file "misc")
                       (:file "newchar")
                       (:file "role")
                       (:file "scenes")
                       (:file "signup")
                       (:file "stage")
                       (:file "view-scene")))
             (:file "sykosomatic")))))
