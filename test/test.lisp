(util:def-file-package #:sykosomatic.test
  (:use :eos)
  (:nicknames :test)
  (:export :sykosomatic :def-test-package))

(def-suite sykosomatic)

(defmacro def-test-package (suite-name &body defpackage-iargs)
  (let ((package-name (concatenate 'string
                                   (string '#:sykosomatic.test.)
                                   (string suite-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog1
           ;; Gotta return the package to make pxref happy.
           (util:def-file-package ,package-name
             (:use :eos :sykosomatic.test)
             ,@defpackage-iargs)
         (def-suite ,suite-name :in sykosomatic)
         (in-suite ,suite-name)))))
(when (find-package :pxref)
  (pushnew 'def-test-package
           (symbol-value (intern "*HANDLE-PACKAGE-FORMS*" :pxref))))
