(util:def-file-package :sykosomatic.template
  (:use :html-template
        :sykosomatic.config)
  (:export :render-template
           :render-page
           :text-input-field
           :select-field
           :field-optgroup))

(defparameter *template-start-marker* "{{")
(defparameter *template-end-marker* "}}")
(defparameter *page-template-relative-path* #p"page.html")
(defparameter *default-template-pathname* *template-path*)
(defparameter *site-page-path* (merge-pathnames "site-pages/" *template-path*))

(defun render-template (template/printer variables)
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template template/printer
                             variables)))

(defun render-page (body-template variables &key title)
  (let ((*ignore-empty-lines* t))
    (render-template *page-template-relative-path*
                     (list* :page-body (list (list (merge-pathnames body-template
                                                                    *site-page-path*)))
                            :title title
                            variables))))

;; Utilities
(defun text-input-field (name label &key id max-length type value error)
  (list #p"text-input-field.html"
        :field-id id :field-name name :field-label label
        :field-value value :field-error error :field-type type
        :field-max-length max-length))

(defun field-optgroup (label &optional options)
  `(:field-optgroup-label ,label :field-optgroup-options ,options))

(defun select-field (name label &key optgroups default-text id class)
  `(#p "select-input-field.html"
       :field-name ,name
       :field-label ,label
       :field-id ,id
       :field-class ,class
       :field-optgroups ,optgroups
       :field-default-text ,default-text))
