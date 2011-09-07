(cl:defpackage #:sykosomatic.character
  (:use :cl :alexandria :sykosomatic.util
        :sykosomatic.db :sykosomatic.entity :postmodern :cl-ppcre)
  (:export :find-character :account-characters
           :create-character :character-account
           :character-account-email
           :character-name :character-description))
(cl:in-package #:sykosomatic.character)

(optimizations)

(defun find-character (name)
  (with-db ()
    (query (:order-by (:select 'entity-id :from 'modifier
                               :where (:and (:= 'type "character-name")
                                            (:= 'text-value name)))
                      (:desc 'precedence))
           :single)))

(defun account-characters (account-id)
  (with-db ()
    (query (:select 'entity-id :from 'modifier
                    :where (:and (:= 'type "character-account")
                                 (:= 'numeric-value account-id)))
           :column)))

;; Validation
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

(defun create-character (account-id name description)
  (with-transaction ()
    (multiple-value-bind (validp errors)
        (validate-new-character name description)
      (if validp
          (with-transaction ()
            (let ((entity (create-entity)))
              (add-modifier entity "character-name" :text-value name)
              (add-modifier entity "character-description" :text-value description)
              (add-modifier entity "character-account" :numeric-value account-id)
              entity))
          (values nil errors)))))

(defun character-name (character-id)
  (text-modifier-value character-id "character-name"))
(defun character-description (character-id)
  (text-modifier-value character-id "character-description"))
(defun character-account (character-id)
  (numeric-modifier-value character-id "character-account"))
(defun character-account-email (character-id)
  (with-db ()
    (with-transaction ()
      (let ((account-id (character-account character-id)))
        (query (:select 'email :from 'account :where (:= 'id account-id)))))))
