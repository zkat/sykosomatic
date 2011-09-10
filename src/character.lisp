(cl:defpackage #:sykosomatic.character
  (:use :cl :alexandria :sykosomatic.util
        :sykosomatic.db :sykosomatic.entity :postmodern :cl-ppcre)
  (:export :find-character :account-characters
           :create-character :character-account
           :character-account-email
           :character-name :character-description
           :cc-features :cc-adjectives
           :cc-location-description
           :cc-select-options))
(cl:in-package #:sykosomatic.character)

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

;; Character creation
(defdao cc-option ()
  ((id :col-type serial :reader id)
   (category :col-type text :initarg :category)
   (short :col-type text :initarg :short)
   (displayed :col-type text :initarg :displayed)
   (details :col-type (or db-null text) :initarg :details))
  (:keys id)
  (:unique category short))

(defdao cc-adjective ()
  ((id :col-type serial :reader id)
   (feature :col-type text :initarg :feature)
   (category :col-type text :initarg :category)
   (adjective :col-type text :initarg :adjective))
  (:keys id)
  (:unique feature category adjective))

(defun cc-features ()
  (with-db ()
    (query (:select 'feature :distinct :from 'cc-adjective)
           :column)))

(defun cc-adjectives (feature-name)
  ;; TODO - can this be done in a single query?
  (with-db ()
    (mapcar (lambda (category)
              (list category (query (:select 'adjective :from 'cc-adjective
                                             :where (:= 'category category))
                                    :column)))
            (query (:order-by
                    (:select 'category :distinct
                             :from 'cc-adjective
                             :where (:= 'feature feature-name))
                    'category)
                   :column))))

(defun cc-location-description (location-name)
  (with-db ()
    (query (:select 'displayed :from 'cc-option
                    :where (:= 'short location-name))
           :single)))

(defun cc-select-options (category)
  (with-db ()
    (query (:select 'short 'displayed 'details
                    :from 'cc-option
                    :where (:= 'category category)))))

;;; Importing seed/test data.
(defun import-adjectives ()
  (let ((data (with-open-file (s (asdf:system-relative-pathname 'sykosomatic "features.txt"))
                (read s)))
        (insert-count 0))
    (with-db ()
      (loop for (feature . cat-and-adjs) in data
         do (loop for (category adjectives) in cat-and-adjs
               do (loop for adjective in adjectives
                     do (ignore-some-conditions (cl-postgres-error:unique-violation)
                          (make-dao 'cc-adjective
                                    :feature feature
                                    :category category
                                    :adjective adjective)
                          (incf insert-count))))))
    insert-count))

(defun import-from-data (category data)
  (with-db ()
    (loop for (short displayed details) in data
       do (make-dao 'cc-option :category category
                    :short short
                    :displayed displayed
                    :details (or details :null)))))

(defparameter *test-data*
  '(("pronoun" . (("she" "She")
                  ("he" "He")
                  ("they" "They")))
    ("origin" . (("local" "Local" "From the Twin Cities area.")
                 ("state" "Minnesotan" "A townie? Maybe from Duluth or something.")
                 ("midwest" "Midwestern" "Hails from elsewhere in the American Midwest.")
                 ("east-coast" "East Coast" "From the east coast of the US.")
                 ("south" "Southern" "Comes from the Southern US.")
                 ("west-coast" "West Coast" "California, Pacific Northwest, etc.")
                 ("else" "Elsewhere" "Alaska, Hawaii, or another country.")))
    ("parents" . (("none" "None")
                  ("one" "One")
                  ("two" "Two")
                  ("more" "More than two")))
    ("siblings" . (("none" "None")
                   ("one" "One")
                   ("two" "Two")
                   ("three" "Three")
                   ("more" "More than three")))
    ("situation" . (("poor" "Poor")
                    ("working-class" "Working Class")
                    ("middle-class" "Middle Class")
                    ("upper-class" "Upper Class")))
    ("career" . (("lumberjack" "Lumberjack")
                 ("programmer" "Software Developer")
                 ("messiah" "Savior")))
    ("friends" . (("ronery" "No friends" "This person is all alone.")
                  ("acquaintances" "A few acquaintances" "Not really, just some acquaintances/coworkers and such.")
                  ("tight" "Just a couple" "Yeah, but just one, or a couple of very close friends.")
                  ("social" "Not a lot of close ones" "Yeah, the character has plenty of friends, but few are really close.")
                  ("loved-by-everyone" "Loved by all" "Yes. The character has a relatively big circle of acquaintances and close friends.")))
    ("significant-other" . (("ronery" "Forever alone" "No, the character is forever alone.")
                            ("dating" "Dating someone" "Kinda, currently seeing someone, but nothing too serious (yet?)")
                            ("serious" "Serious relationship" "Yes. The character has been with someone for a while.")
                            ("ball-and-chain" "Married/Partnered" "Yes, the character is in a committed relationship and/or married.")))
    ("location" . (("midway" "Midway Area")
                   ("downtown" "Downtown Minneapolis")
                   ("dinkytown" "Dinkytown Neighborhood")
                   ("riverfront" "Riverfront District")
                   ("west-bank" "West Bank Neighborhood")))))

(defun import-test-form-options ()
  (loop for (category . data) in *test-data*
     do (import-from-data category data)))
