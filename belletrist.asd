;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:yaclml #:clws #:bordeaux-threads #:jsown)
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "belletrist")))))

