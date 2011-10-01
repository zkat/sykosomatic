(util:def-file-package #:sykosomatic.command.social
  (:use :sykosomatic.command
        :sykosomatic.game-objects.nameable))

(defcommand social ()
  (tell-local *actor* "~A ~A."
              (full-name *actor*)
              *verb*))