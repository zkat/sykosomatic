(util:def-file-package #:sykosomatic.test
  (:use :eos)
  (:nicknames :test)
  (:export :sykosomatic :def-test-package))

(def-suite sykosomatic)

(defmacro def-test-package (suite-name &body defpackage-iargs)
  (let ((package-name (concatenate 'string
                                   (string '#:sykosomatic.test.)
                                   (string suite-name))))
    `(progn
      (util:def-file-package ,package-name
        (:use :eos :sykosomatic.test)
        ,@defpackage-iargs)
      (def-suite ,suite-name :in sykosomatic)
      (in-suite ,suite-name))))
