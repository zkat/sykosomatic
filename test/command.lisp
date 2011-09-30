(cl:defpackage #:sykosomatic.test.command
  (:use :cl :alexandria :eos
        :sykosomatic.test
        :sykosomatic.command))
(cl:in-package #:sykosomatic.test.command)

(def-suite command :in sykosomatic)

(in-suite command)

(test basic
  (unwind-protect
       (let ((success (gensym)))
         (defcommand test
           success)
         (add-verb-command "smiles" 'test)
         (is (eq success (funcall (verb-command "smiles")))))
    (remove-verb-command "smiles")
    (remove-command 'test)))
