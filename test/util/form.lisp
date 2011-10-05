(test:def-test-package util.form
  (:use :sykosomatic.util.form))

(defun check-required (field-value)
  (check-field field-value "is required."))
(defun validate-text-field (text)
  text)
(defun validate-integer-field (int-text &aux *read-eval*)
  (check-required int-text)
  (let ((integer (ignore-errors (parse-integer int-text))))
    (check-field (numberp integer) "~~A is not a number")
    integer))
(defun validate-text-field-with-args (text &key max-length)
  (validate-text-field text)
  (check-field (>= max-length (length text)) "~~A must be below ~A characters." max-length )
  text)

(deform test-form ()
  ((text-field 'validate-text-field)
   (processed-field 'validate-integer-field)
   (text-field-with-args 'validate-text-field-with-args :max-length 20)))

(test deform)
(test check-field)

(test make-form
  (finishes (make-form 'test-form))
  (finishes (make-form 'test-form '(("text-field" . "foo")
                                    ("processed-field" . "42")
                                    ("text-field-with-args" . "bwahahhahahahahhahahahahhahahah")))))

(test form-valid-p)
(test form-errors)

(test field-raw-value
  (let ((form (make-form 'test-form '(("email" . "foo@bar.com")
                                      ("display-name" . "display-name")
                                      ("password" . "foobarbaz")
                                      ("confirmation" . "foobarbaz")
                                      ("favorite-number" . "42")))))
    (is (string= "foo@bar.com" (field-raw-value form 'email)))
    (is (string= "42" (field-raw-value form 'favorite-number)))))

(test field-value
  (let ((form (make-form 'test-form '(("email" . "foo@bar.com")
                                      ("display-name" . "display-name")
                                      ("password" . "foobarbaz")
                                      ("confirmation" . "foobarbaz")
                                      ("favorite-number" . "42")))))
    (is (string= "foo@bar.com" (field-value form 'email)))
    (is (eql 42 (field-value form 'favorite-number)))))

(test field-error)
(test field-error-format-string)
