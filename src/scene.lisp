(cl:defpackage #:sykosomatic.scene
  (:use :cl :alexandria :chillax.core :sykosomatic.db)
  (:export :create-scene :add-action :find-scenes-by-account-name :find-scene-with-actions :find-scene-actions
           :scene-id :scene-upvote :scene-rating))
(cl:in-package #:sykosomatic.scene)

(defun ensure-scene-design-doc ()
  (ensure-doc "_design/scene"
              (mkdoc "language" "common-lisp"
                     "views" (mkdoc "by_account_name"
                                    (mkdoc "map"
                                           (mapfun doc "scene"
                                             (emit (string-downcase (hashget doc "account_name"))
                                                   doc)))
                                    "full_by_id"
                                    (mkdoc "map"
                                           (couchfun (doc &aux (type (hashget doc "type")))
                                             (cond ((string= type "scene")
                                                    (emit (list (hashget doc "_id") 0) doc))
                                                   ((string= type "scene-action")
                                                    (emit (list (hashget doc "scene_id")
                                                                1
                                                                (hashget doc "timestamp"))
                                                          doc))
                                                   (t nil))))
                                    "actions_by_scene_id"
                                    (mkdoc "map"
                                           (mapfun doc "scene-action"
                                             (emit (hashget doc "scene_id")
                                                   doc)))
                                    "rating_by_scene_id"
                                    (mkdoc "map"
                                           (mapfun doc "scene-rating"
                                             (emit (hashget doc "scene_id") 1))
                                           "reduce"
                                           (couchfun (keys vals &optional rrp)
                                             (declare (ignore keys rrp))
                                             (reduce #'+ vals :initial-value 0)))
                                    "rating_by_account_name"
                                    (mkdoc "map"
                                           (mapfun doc "scene-rating"
                                             (emit (list (hashget doc "scene_id")
                                                         (hashget doc "account_name")) doc)))))))

(defun create-scene (account-name)
  (let ((uuid (get-uuid)))
    (ensure-doc uuid
                (mkdoc "type" "scene"
                       "account_name" account-name))
    uuid))

(defun add-action (scene-id &key character action dialogue timestamp)
  (ensure-doc (get-uuid)
              (mkdoc "type" "scene-action"
                     "scene_id" scene-id
                     "character" character
                     "action" action
                     "dialogue" dialogue
                     "timestamp" timestamp)))

(defun scene-id (scene)
  (doc-val scene "_id"))

(defun find-scene-rating-by-account-name (scene-id account-name)
  (view-query-value "scene" "rating_by_account_name" (list scene-id account-name)))

(defun scene-upvote (scene-id account-name)
  (unless (find-scene-rating-by-account-name scene-id account-name)
    (ensure-doc (get-uuid)
                (mkdoc "type" "scene-rating"
                       "scene_id" scene-id
                       "account_name" account-name))
    t))

(defun scene-rating (scene-id)
  (view-query-value "scene" "rating_by_scene_id" scene-id))

(defun find-scenes-by-account-name (account-name)
  (view-query-value "scene" "by_account_name" account-name nil))

(defun find-scene-with-actions (scene-id)
  (when-let ((results (doc-val (query-view *db* "scene" "full_by_id"
                                           :startkey (list scene-id)
                                           :endkey (list scene-id 2))
                               "rows")))
    (mapcar (rcurry #'doc-val "value") results)))

(defun find-scene-actions (scene-id)
  (view-query-value "scene" "actions_by_scene_id" scene-id nil))
