(cl:defpackage #:sykosomatic.character
  (:use :cl :alexandria :sykosomatic.db :chillax.core :cl-ppcre)
  (:export :find-character :find-characters-by-account-name
           :ensure-character-design-doc :create-character
           :character-name :character-description))
(cl:in-package #:sykosomatic.character)

(defun ensure-character-design-doc ()
  (ensure-doc "_design/character"
              (mkdoc "language" "common-lisp"
                     "views" (mkdoc "by_account_name"
                                    (mkdoc "map"
                                           (mapfun doc "character"
                                             (emit (string-downcase (hashget doc "account_name"))
                                                   doc)))
                                    "by_name"
                                    (mkdoc "map"
                                           (mapfun doc "character"
                                             (emit (string-downcase (hashget doc "name"))
                                                   doc)))))))

(defun find-character (name)
  (view-query-value "character" "by_name" (string-downcase name)))

(defun find-characters-by-account-name (account-name)
  (view-query-value "character" "by_account_name" (string-downcase account-name) nil))

(defparameter *character-name-regex* (create-scanner "^[A-Z0-9._.-]+$"
                                                     :case-insensitive-mode t))

(defun valid-character-name-p (name)
  ;; TODO - First character in name should be uppercase, all others downcase. (maybe?)
  (when (and (>= (length name) 4)
             (<= (length name) 24)
             (scan *character-name-regex* name))
    t))

(defun validate-new-character (name description)
  (with-validation
    (assert-required "Character name" name)
    (assert-validation (valid-character-name-p name) "Invalid character name.")
    (assert-validation (not (find-character name)) "Character with that name already exists.")
    (assert-validation (<= (length description) 500) "Descriptions must be under 500 characters long.")))

(defun create-character (account-name name description)
  (multiple-value-bind (validp errors)
      (validate-new-character name description)
    (if validp
        (ensure-doc (get-uuid)
                    (mkdoc "type" "character"
                           "account_name" account-name
                           "name" name
                           "description" description))
        (values nil errors))))

(defun character-name (character)
  (doc-val character "name"))
(defun character-description (character)
  (doc-val character "description"))