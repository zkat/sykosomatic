(util:def-file-package #:sykosomatic.handler.index
  (:use :hunchentoot
        :sykosomatic.template)
  (:export :index))

(define-easy-handler (index :uri "/") ()
  (render-page "index.html" nil :title "Sykosomatic Test Site"))
