(util:def-file-package :sykosomatic.util.form
  (:use :cl-ppcre)
  (:export :deform
           :*form*
           :check-field
           :make-form
           :form-valid-p
           :form-errors
           :field-raw-value
           :field-value
           :field-error))

(defvar *form-definitions* (make-hash-table :test #'eq))
(defun ensure-form (name field-definitions)
  (setf (gethash name *form-definitions*)
        (let ((form-hash (make-hash-table :test #'equalp)))
          (loop for (field-name validator validator-args type) in field-definitions
             do (setf (gethash field-name form-hash)
                      (list validator validator-args type)))
          form-hash))
  name)
(defun find-form-def (name)
  (if-let (def (gethash name *form-definitions*))
    def
    (error "No definition form form: ~A" name)))

(defmacro deform (name supers field-defs)
  (declare (ignore supers))
  `(ensure-form ',name
                (list
                 ,@(loop for (name validator . validator-args) in field-defs
                      collect `(list ',(if (consp name) (car name) name) ,validator
                                     ,(when validator-args
                                            `(lambda ()
                                               (list ,@validator-args)))
                                     ',(if (consp name) (second name) 'atom))))))

;; These two are pretty much taken from hunchentoot.
(defun collect-list-parameter (target-name parameter-alist)

  (loop for (name . value) in parameter-alist
     when (string-equal name target-name)
     collect value))

(defun collect-array-parameter (target-name parameter-alist)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameter-alist
             for index = (register-groups-bind (name index-string)
                             ("^(.*)\\[(\\d+)\\]$" full-name)
                           (when (string-equal name target-name)
                             (let (*read-eval*)
                               (ignore-some-conditions (parse-error)
                                 (parse-integer index-string)))))
             when index
             collect (cons index value)))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defparameter *validp* (gensym "VALIDP"))
(defvar *form*)
(defun bind-form (form-def form bindings)
  (setf (gethash *validp* form) t)
  (maphash (lambda (name field-def)
             (let ((type (third field-def)))
               (setf (gethash name form)
                     (list (case type
                                  (array (collect-array-parameter name bindings))
                                  (list (collect-list-parameter name bindings))
                                  (atom
                                   (cdr (assoc name bindings :test #'string-equal)))
                                  (otherwise (error "Unknown parameter type: ~A" type)))))))
           form-def)
  (maphash (lambda (name field-def)
             (destructuring-bind (validator validator-arg-function . ig) field-def
               (declare (ignore ig))
               (let ((raw-value (field-raw-value form name)))
                 (setf (gethash name form)
                       (multiple-value-bind (validated-value error)
                           (handler-case
                               (let ((*form* form))
                                 (apply validator (if validator-arg-function
                                                      (cons raw-value
                                                            (funcall validator-arg-function))
                                                      (list raw-value))))
                             (validation-error (e)
                               (values nil (validation-error-message e))))
                         (when error
                           (setf (gethash *validp* form) nil))
                         (list raw-value validated-value error))))))
           form-def))

(define-condition validation-error (error)
  ((msg :initarg :msg :reader validation-error-message)))

(defun check-field (test error-format &rest error-format-args)
  (unless test
    (error 'validation-error :msg (apply #'format nil error-format error-format-args))))

(defun make-form (form-class &optional (binding-alist nil got-bindings-p))
  (let ((def (find-form-def form-class))
        (form (make-hash-table :test #'eq)))
    (if got-bindings-p
        (bind-form def form binding-alist)
        (setf (gethash *validp* form) nil))
    form))

(defun field-raw-value (form field-name)
  (car (gethash field-name form)))
(defun field-value (form field-name)
  (cadr (gethash field-name form)))
(defun form-valid-p (form)
  (values (gethash *validp* form)))
(defun field-error (form field-name)
  (caddr (gethash field-name form)))
(defun form-errors (form &aux errors)
  (maphash (lambda (field-name field-values)
             (unless (eq *validp* field-name)
               (when-let (error-string (caddr field-values))
                 (push (cons field-name error-string) errors))))
           form)
  (nreverse errors))
