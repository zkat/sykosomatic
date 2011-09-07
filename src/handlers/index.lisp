(cl:defpackage #:sykosomatic.handler.index
  (:use :hunchentoot)
  (:export :index))
(cl:in-package #:sykosomatic.handler.index)

(define-easy-handler (index :uri "/") ()
  (templ:render-template "index"))
