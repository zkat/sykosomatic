(test:def-test-package command
  (:use :sykosomatic.command))

(test basic
  (unwind-protect
       (let ((success (gensym)))
         (defcommand test
           success)
         (add-verb-command "smiles" 'test)
         (is (eq success (invoke-verb-command :verb "smiles"))))
    (remove-verb-command "smiles")
    (remove-command 'test)))
