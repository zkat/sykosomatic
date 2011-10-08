(util:def-file-package #:sykosomatic.handler.index
  (:use :hunchentoot
        :sykosomatic.handler)
  (:export :index))

(define-easy-handler (index :uri "/") ()
  (render-page "index.html" '(:pre-content ((#p"site-pages/index-pre-content.html")))
               :title "Sykosomatic Test Site"))
