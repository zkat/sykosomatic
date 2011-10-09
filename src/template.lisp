(util:def-file-package :sykosomatic.template
  (:use :html-template
        :sykosomatic.config)
  (:export :render-template
           :text-input-field
           :select-field
           :field-optgroup))

(defparameter *template-start-marker* "{{")
(defparameter *template-end-marker* "}}")
(defparameter *default-template-pathname* *template-path*)
(defparameter *string-modifier* #'escape-string-minimal-plus-quotes)

(defun render-template (template/printer variables)
  (let ((*ignore-empty-lines* t))
    (with-output-to-string (*default-template-output*)
      (fill-and-print-template template/printer
                               variables))))

;; Utilities
(defun text-input-field (name label &key id max-length type value error)
  (list #p"text-input-field.html"
        :field-id id :field-name name :field-label label
        :field-value value :field-error error :field-type type
        :field-max-length max-length))

(defun field-optgroup (label &optional options)
  `(:field-optgroup-label ,label :field-optgroup-options ,options))

(defun select-field (name label &key optgroups default-text id class error value)
  `(#p "select-input-field.html"
       :field-name ,name
       :field-label ,label
       :field-id ,id
       :field-class ,class
       :field-optgroups ,(loop for optgroup in optgroups
                            collect (field-optgroup (getf optgroup :field-optgroup-label)
                                                    (maybe-mark-selected value (getf optgroup :field-optgroup-options))))
       :field-default-text ,default-text
       :field-error ,error
       :field-value ,value))

(defun maybe-mark-selected (target-value options)
  (loop for opt in options
     collect (if (equal target-value (getf opt :option-value))
                 (list* :option-selected-p t opt)
                 opt)))
