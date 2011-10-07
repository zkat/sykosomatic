(util:def-file-package #:sykosomatic.handler.index
  (:use :hunchentoot
        :sykosomatic.handler)
  (:export :index))

(define-easy-handler (index :uri "/") ()
  (render-page "index.html" nil :title "Sykosomatic Test Site"))
