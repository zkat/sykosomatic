(cl:defpackage #:sykosomatic.handler.view-scene
  (:use :cl :hunchentoot
        :sykosomatic.handler
        :sykosomatic.session
        :sykosomatic.scene)
  (:export :view-scene))
(cl:in-package #:sykosomatic.handler.view-scene)

(define-easy-handler (view-scene :uri "/view-scene") ((id :parameter-type 'integer))
  (case (request-method*)
    (:get
     (cond ((scene-exists-p id)
            (with-form-errors
              (templ:render-template "view-scene" id (not (null (current-account))) (scene-rating id))))
           (t (push-error "No scene with ID ~A." id)
              (redirect "/scenes"))))
    (:post
     (ensure-logged-in)
     (cond ((account-voted-p id (current-account))
            (push-error "You've already voted for this scene.")
            (redirect "/view-scene"))
           (t
            (scene-upvote id (current-account))
            (redirect (format nil "/view-scene?id=~A" id)))))))
