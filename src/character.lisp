(cl:defpackage #:sykosomatic.character
  (:use :cl :alexandria :cl-ppcre
        :sykosomatic.util
        :sykosomatic.db
        :sykosomatic.entity)
  (:export :find-character :account-characters
           :create-character :character-account
           :character-account-email
           :character-name :character-description
           :cc-features :cc-adjectives
           :cc-location-description
           :cc-select-options))
(cl:in-package #:sykosomatic.character)

(defun find-character (name)
  (find-by-modifier-value 'nickname name :test :ilike))

(defun account-characters (account-id)
  (find-by-modifier-value 'account account-id :allp t))

;; Validation
(defparameter *character-name-regex* (create-scanner "^[A-Z'-]+$"
                                                     :case-insensitive-mode t))

(defun valid-character-name-p (name)
  (when (and (>= (length name) 4)
             (<= (length name) 24)
             (scan *character-name-regex*
                   (with-db () (query (:select (:unaccent name)) :single))))
    t))

(defun validate-new-character (name)
  (with-validation
    (assert-required "Nickname" name)
    (assert-validation (valid-character-name-p name) "Invalid nickname.")
    (assert-validation (not (find-character name)) "Character with that name already exists.")))

(defdao cc-values ()
  ((entity-id bigint)
   (pronoun text)
   (first-name text)
   (nickname text)
   (last-name text)
   (origin text)
   (parents text)
   (siblings text)
   (friends text)
   (so text)
   (where text))
  (:keys id))

(defdao cc-career-info ()
  ((cc-values-id bigint)
   (career text)
   (years-spent numeric))
  (:keys id))

(defdao cc-feature-info ()
  ((cc-values-id bigint)
   (feature text)
   (adjective text))
  (:keys id))

(defun create-character (account-id &key
                         pronoun first-name nickname last-name
                         origin parents siblings situation friends
                         so career-info feature-info where)
  (with-db ()
    (with-transaction ()
      (multiple-value-bind (validp errors)
          (validate-new-character nickname)
        (if validp
            (with-transaction ()
              (let ((entity (create-entity)))
                (add-modifier entity 'nickname nickname)
                (add-modifier entity 'account account-id)
                (let ((cc-values-id (id (make-dao 'cc-values
                                                  :entity-id entity
                                                  :pronoun  pronoun
                                                  :first-name first-name
                                                  :nickname nickname
                                                  :last-name last-name
                                                  :origin origin
                                                  :parents parents
                                                  :siblings siblings
                                                  :situation situation
                                                  :friends friends
                                                  :so so
                                                  :where where))))
                  (loop for (career . years-spent) in career-info
                     when years-spent
                     do (make-dao 'cc-career-info :cc-values-id cc-values-id
                                  :career career :years-spent years-spent))
                  (loop for (feature . adjective) in feature-info
                     when adjective
                     do (make-dao 'cc-feature-info :cc-values-id cc-values-id
                                  :feature feature :adjective adjective)))
                entity))
            (values nil errors))))))

(defun character-name (character-id)
  (modifier-value character-id 'nickname))
(defun character-description (character-id)
  (modifier-value character-id 'description))
(defun character-account (character-id)
  (modifier-value character-id 'account))
(defun character-account-email (character-id)
  (with-db ()
    (with-transaction ()
      (let ((account-id (character-account character-id)))
        (query (:select 'email :from 'account :where (:= 'id account-id)))))))

;; Character creation
(defdao cc-option ()
  ((category text)
   (short text)
   (displayed text)
   (details (or db-null text)))
  (:keys id)
  (:unique category short))

(defdao cc-adjective ()
  ((feature text)
   (category text)
   (adjective text))
  (:keys id)
  (:unique feature category adjective))

(defun cc-features ()
  (with-db ()
    (query (:select 'feature :distinct :from 'cc-adjective)
           :column)))

(defun cc-adjectives (feature-name)
  (with-db ()
    (query (:select 'category (:as (:array-agg 'adjective) 'adjectives)
                    :from 'cc-adjective
                    :where (:= 'feature feature-name)
                    :group-by 'category))))

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
     do (import-from-data category data))
  (import-adjectives))
