(util:def-file-package :sykosomatic.util.form
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
          (loop for (field-name validator validator-args) in field-definitions
             do (setf (gethash field-name form-hash)
                      (list validator validator-args)))
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
                      collect `(list ',name ,validator ,(when validator-args
                                                          `(lambda ()
                                                             (list ,@validator-args))))))))

(defparameter *validp* (gensym "VALIDP"))
(defvar *form*)
(defun bind-form (form-def form bindings)
  (setf (gethash *validp* form) t)
  (maphash (lambda (name field-def)
             (destructuring-bind (validator validator-arg-function) field-def
               (let ((raw-value (cdr (assoc name bindings :key #'string :test #'string-equal))))
                 (setf (gethash name form)
                       (list raw-value nil nil))
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