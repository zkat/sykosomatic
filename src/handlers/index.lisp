(util:def-file-package #:sykosomatic.handler.index
  (:use :hunchentoot)
  (:export :index))

(define-easy-handler (index :uri "/") ()
  (templ:render-template "index"))
