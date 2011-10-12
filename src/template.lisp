(util:def-file-package :sykosomatic.template
  (:use :html-template
        :sykosomatic.util.form
        :sykosomatic.config)
  (:export :render-template
           :text-field
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
(defun labelify (dashed-string)
  (format nil "~:(~A~)" (substitute #\space #\- dashed-string)))

(defun text-field (form field-name &key
                   (name (string-downcase field-name))
                   (label (labelify (string field-name)))
                   (id (concatenate 'string (string-downcase field-name) "-field"))
                   max-length
                   class
                   (type "text")
                   (template #p"fields/text.html")
                   (value (field-raw-value form field-name))
                   (error (field-error form field-name)))
  (list template
        :field-id id :field-name name :field-label label
        :field-value value
        :field-error error
        :field-type type
        :field-max-length max-length
        :field-class class))

(defun field-optgroup (label &optional options)
  `(:field-optgroup-label ,label :field-optgroup-options ,options))

(defun select-field (form field-name &key
                     (name (string-downcase field-name))
                     (label (englishify (string field-name)))
                     (id (concatenate 'string (string-downcase field-name) "-field"))
                     class
                     (template #p"fields/select.html")
                     (value (field-raw-value form field-name))
                     (error (field-error form field-name))
                     optgroups
                     default-text)
  (list template
        :field-name name
        :field-label label
        :field-id id
        :field-class class
        :field-optgroups (loop for optgroup in optgroups
                            collect (field-optgroup (getf optgroup :field-optgroup-label)
                                                    (maybe-mark-selected value (getf optgroup :field-optgroup-options))))
        :field-default-text default-text
        :field-error error
        :field-value value))

(defun maybe-mark-selected (target-value options)
  (loop for opt in options
     collect (if (equal target-value (getf opt :option-value))
                 (list* :option-selected-p t opt)
                 opt)))
