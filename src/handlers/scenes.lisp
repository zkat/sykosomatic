(util:def-file-package #:sykosomatic.handler.scenes
  (:use :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.scene
        :sykosomatic.account)
  (:export :scenes))

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  #+nil
  (with-form-errors
    (templ:render-template "scenes" (mapcar #'scene-id (find-scenes-by-account-id (current-account))))))

