(cl:defpackage #:sykosomatic.scene
  (:use :cl :chillax.core :sykosomatic.db)
  (:export :create-scene))
(cl:in-package #:sykosomatic.scene)

(defun ensure-scene-design-doc ()
  (ensure-doc "_design/scene"
              (mkdoc "language" "common-lisp"
                     "views" (mkdoc "by_account_name"
                                    (mkdoc "map"
                                           (mapfun doc "scene"
                                             (emit (string-downcase (hashget doc "account_name"))
                                                   doc)))))))
(defun create-scene (account-name)
  (ensure-doc (get-uuid)
              (mkdoc "type" "scene"
                     "account_name" account-name)))
