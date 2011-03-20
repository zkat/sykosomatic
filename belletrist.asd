;;;; belletrist.asd

(asdf:defsystem #:belletrist
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot #:yaclml #:clws
               #:bordeaux-threads
               #:jsown
               #:chillax.core #:chillax.jsown)
  :components
  ((:module src
            :serial t
            :components
            ((:file "account")
             (:file "belletrist")))))

