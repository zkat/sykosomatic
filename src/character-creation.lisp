(util:def-file-package #:sykosomatic.character-creation
  (:use :cl-ppcre
        :sykosomatic.db
        :sykosomatic.entity
        :sykosomatic.account
        :sykosomatic.components.describable
        :sykosomatic.util.form)
  (:export :pronoun
           :growing-up
           :career
           :*max-careers*
           :relationships
           :features
           :*max-features*
           :location
           :name-and-confirmation
           :newchar :create-character
           :cc-features :cc-adjectives
           :cc-location-description
           :cc-select-options))

;;; Util
(defun field-required (validator)
  (lambda (&rest args)
    (check-field (not (emptyp (car args))) "Field is required.")
    (apply validator args)))

(defun cc-option-validator (option &optional (error-msg "Invalid option."))
  (lambda (val)
    (check-field (find val (cc-select-options option)
                       :key #'cadr
                       :test #'equal)
                 error-msg)
    val))

;; Character creation forms
(deform pronoun ()
  ((:pronoun (field-required
              (cc-option-validator "pronoun")))))

(deform growing-up ()
  ((:origin (field-required (cc-option-validator "origin")))
   (:parents (field-required (cc-option-validator "parents")))
   (:siblings (field-required (cc-option-validator "siblings")))
   (:finances (field-required (cc-option-validator "situation")))))

(defparameter *max-age* 80)
(defparameter *start-age* 18)
(defparameter *max-careers* 5)
(deform career ()
  (((:careers array) (lambda (all-careers)
                       (check-field (= *max-careers* (length all-careers)) "Huh?")
                       (check-field (some (compose #'not #'emptyp) all-careers) "Must choose at least one career.")
                       (let ((option-validator (cc-option-validator "career" "Invalid career.")))
                         (map nil (lambda (career)
                                    (unless (emptyp career)
                                      (funcall option-validator career)))
                              all-careers))
                       all-careers))
   ((:career-times array) (lambda (career-times)
                            (let ((times
                                   (coerce
                                    (loop
                                       for career across (field-raw-value *form* :careers)
                                       for time across career-times
                                       collect
                                       (let ((parsed-time (ignore-some-conditions (parse-error)
                                                            (parse-integer (string-trim '(#\space) time)))))
                                         (check-field (if (emptyp career) t parsed-time)
                                                      "Must select a valid time span for each selected career.")
                                         parsed-time))
                                    'array)))
                              (check-field (>= *max-age* (+ *start-age*
                                                            (reduce #'+ (remove nil times)
                                                                    :initial-value 0)))
                                           "Can't create a character older than ~A years." *max-age*))))))

(deform relationships ()
  ((:friends (field-required (cc-option-validator "friends")))
   (:romance (field-required (cc-option-validator "significant-other")))))

(defparameter *max-features* 5)
(defun validate-features (all-features)
  (check-field (= *max-features* (length all-features)) "Huh?")
  (let ((cc-features (cc-features)))
    (map nil (lambda (feature)
               (unless (emptyp feature)
                 (check-field (find feature cc-features :test #'string=) "Got an unexpected feature.")))
         all-features))
  all-features)
(defun validate-feature-adjs (all-adjs)
  (loop
     for feature across (field-raw-value *form* :features)
     for adjective across all-adjs
     do (check-field (if (emptyp feature) t (not (emptyp adjective)))
                     "You must select an adjective for each feature added.")
       (check-field (if (emptyp adjective) t (not (emptyp feature)))
                    "Huh?")
       (unless (emptyp feature)
         (check-field (cc-adjectives feature) "That adjective is not valid for that feature.")))
  all-adjs)

(deform features ()
  (((:features array) #'validate-features)
   ((:feature-adjs array) #'validate-feature-adjs)))

(deform location ()
  ((:where (field-required (cc-option-validator "location")))))

(defparameter *character-name-regex* (create-scanner "^[A-Z', .\\-]+$"
                                                     :case-insensitive-mode t))

(defun validate-full-name (name)
  (check-field (and (<= 1 (length name) 30)) "Full name must be between 1 and 30 characters long.")
  (check-field (scan *character-name-regex*
                     (db-query (:select (:unaccent name)) :single))
               "Invalid name.")
  name)

(defun validate-nickname (name)
  (check-field (and (<= 1 (length name) 15)) "Nickname must be between 1 and 15 characters long.")
  (check-field (scan *character-name-regex*
                     (db-query (:select (:unaccent name)) :single))
               "Invalid name.")
  name)

(deform name-and-confirmation ()
  ((:full-name (field-required #'validate-full-name))
   (:nickname (field-required #'validate-nickname))))

;; Validation

(defun valid-character-name-p (name)
  (when (and (>= (length name) 4)
             (<= (length name) 24)
             (scan *character-name-regex*
                   (db-query (:select (:unaccent name)) :single)))
    t))

(defun validate-new-character (name)
  (with-validation
    (assert-required "Nickname" name)
    (assert-validation (valid-character-name-p name) "Invalid nickname.")))

(deform newchar ()
  ((:pronoun #'identity)
   (:name (field-required #'identity))
   (:origin #'identity)
   (:parents #'identity)
   (:siblings #'identity)
   (:situation #'identity)
   (:friends #'identity)
   (:so #'identity)
   ((:careers array) #'identity)
   ((:career-times array) #'identity)
   ((:features array) #'identity)
   ((:feature-adjs array) #'identity)
   (:where #'identity)))

(defun create-character (account-id form)
  (with-transaction ()
    (let ((entity (create-entity)))
      (configure-noun entity "person" :plural "people")
      (configure-nickname entity entity (field-value form :name))
      (add-body entity account-id)
      #+nil
      (let ((cc-values-id (insert-row 'cc-values
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
                                      :where where)))
        (loop for (career . years-spent) in career-info
           when years-spent
           do (insert-row 'cc-career-info :cc-values-id cc-values-id
                          :career career :years-spent years-spent))
        (loop for (feature . adjective) in feature-info
           when adjective
           do (insert-row 'cc-feature-info :cc-values-id cc-values-id
                          :feature feature :adjective adjective)))
      entity)))

;; newchar parameter storage
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

(defdao cc-option ()
  ((category text)
   (short text)
   (displayed text)
   (details (or db-null text)))
  (:keys id)
  (:unique (category short)))

(defdao cc-adjective ()
  ((feature text)
   (category text)
   (adjective text))
  (:keys id)
  (:unique (feature category adjective)))

(defun cc-features ()
  (db-query (:select 'feature :distinct :from 'cc-adjective)
            :column))

(defun cc-adjectives (feature-name)
  (db-query (:select 'category (:as (:array-agg 'adjective) 'adjectives)
                     :from 'cc-adjective
                     :where (:= 'feature feature-name)
                     :group-by 'category)))

(defun cc-location-description (location-name)
  (db-query (:select 'displayed :from 'cc-option
                     :where (:= 'short location-name))
            :single))

(defun cc-select-options (category)
  (db-query (:select (:as 'short 'option-value)
                     (:as 'displayed 'option-text)
                     :from 'cc-option
                     :where (:= 'category category))
            :plists))

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
                          (insert-row 'cc-adjective
                                      :feature feature
                                      :category category
                                      :adjective adjective)
                          (incf insert-count))))))
    insert-count))

(defun import-from-data (category data)
  (with-db ()
    (loop for (short displayed details) in data
       do (insert-row 'cc-option :category category
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

(register-db-init-hook 'character-creation 'import-test-form-options)
