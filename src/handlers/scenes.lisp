(cl:defpackage #:sykosomatic.handler.scenes
  (:use :cl :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.scene
        :sykosomatic.account)
  (:export :scenes))
(cl:in-package #:sykosomatic.handler.scenes)

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:render-template "scenes" (mapcar #'scene-id (find-scenes-by-account-id (current-account))))))

