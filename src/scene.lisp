(cl:defpackage #:sykosomatic.scene
  (:use :cl :chillax.core :sykosomatic.db))
(cl:in-package #:sykosomatic.scene)

(defun create-scene ()
  (ensure-doc (get-uuid)
              (mkdoc "type" "scene")))
