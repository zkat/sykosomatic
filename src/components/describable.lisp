(util:def-file-package #:sykosomatic.components.describable
  (:use :sykosomatic.db
        :sykosomatic.entity)
  (:export :noun
           :plural-noun
           :pluralize
           :adjectives
           :add-feature
           :remove-feature
           :list-features
           :nickname
           :base-description
           :short-description
           :find-by-short-description))

;;;
;;; Nouns
;;;
(defdao noun ()
  ((entity-id bigint)
   (singular text)
   (plural text)))

(defun noun (entity)
  (values (db-query (:select 'singular :from 'noun :where (:= 'entity-id entity)) :single)))
(defun plural-noun (entity)
  (values (db-query (:select 'plural :from 'noun :where (:= 'entity-id entity)) :single)))
(defun pluralize (word &aux (unaccented-word (db-query (:select (:unaccent word)) :single)))
  (cond ((or
          (ends-with-subseq "ch" unaccented-word :test #'equal)
          (ends-with-subseq "sh" unaccented-word :test #'equal)
          (ends-with-subseq "s" unaccented-word :test #'equal))
         (format nil "~Aes" word))
        ((and (equal #\y (last-elt unaccented-word))
              (> (length unaccented-word) 2)
              (not (find (elt unaccented-word (- (length unaccented-word) 2))
                         "aeiou")))
         (format nil "~Aies" (subseq word 0 (1- (length word)))))
        ((and (equal #\o (last-elt unaccented-word))
              (> (length unaccented-word) 2)
              (not (find (elt unaccented-word (- (length unaccented-word) 2))
                         "aeiou")))
         (format nil "~Aoes" (subseq word 0 (1- (length word)))))
        (t (format nil "~As" word))))

(defun (setf noun) (new-value entity)
  (with-transaction ()
    (cond ((null new-value)
           (db-query (:delete-from 'noun :where (:= 'entity-id entity))))
          ((noun entity)
           (db-query (:update 'noun :set
                              'singular (if (consp new-value) (car new-value) new-value)
                              'plural (if (consp new-value) (cadr new-value) (pluralize new-value))
                              :where (:= 'entity-id entity))))
          (t
           (insert-row 'noun
                       :entity-id entity
                       :singular (if (consp new-value) (car new-value) new-value)
                       :plural (if (consp new-value) (cadr new-value) (pluralize new-value)))))
    (cache-base-description entity :update-featured t))
  new-value)

;;;
;;; Adjectives
;;;
(defdao adjective ()
  ((entity-id bigint)
   (adjective text)))

(defun adjectives (entity)
  (db-query (:select 'adjective :from 'adjective :where (:= 'entity-id entity))
            :column))
(defun (setf adjectives) (new-value entity)
  (with-transaction ()
    (db-query (:delete-from 'adjective :where (:= 'entity-id entity)))
    (map nil (lambda (new-adj) (insert-row 'adjective :entity-id entity :adjective new-adj))
         new-value)
    (cache-base-description entity :update-featured t))
  new-value)

;;;
;;; Features
;;;
;;; - Features are basically regular entities, with their own nouns, adjectives, and features, that
;;;   are treated as 'details' for this entity.
;;;
(defdao feature ()
  ((entity-id bigint)
   (feature-id bigint)))

(defun add-feature (entity feature)
  (with-transaction ()
    (unless (db-query (:select t :from 'feature :where (:and (:= 'entity-id entity)
                                                             (:= 'feature-id feature)))
                      :single)
      (insert-row 'feature :feature-id feature :entity-id entity)
      (cache-base-description entity))))
(defun remove-feature (entity feature)
  (with-transaction ()
    (db-query (:delete-from 'feature :where (:and (:= 'entity-id entity)
                                                  (:= 'feature-id feature))))
    (cache-base-description entity)))
(defun list-features (entity)
  (db-query (:select 'feature-id :from 'feature :where (:= 'entity-id entity))
            :column))

;;;
;;; Base description
;;;
;;; - The base description for an entity is what it will look like when not given a nickname. For
;;;   example, 'a teapot', or 'a tall person'.
;;;
;;;   We cache these base descriptions to make lookups fast. These caches are updated whenever a
;;;   feature, noun, or adjective changes. In the case of nouns and adjectives, any entities that
;;;   use the current entity as a feature also have their cache updated. This system needs to go no
;;;   deeper than a single level because base-description does not recursively 'render' features.
(defdao cached-base-description ()
  ((entity-id bigint)
   (description text)))

(defun cache-base-description (entity &key update-featured)
  (let ((new-description (calculate-base-description entity)))
    (cond ((null new-description)
           (db-query (:delete-from 'cached-base-description
                                   :where (:= 'entity-id entity))))
          ((db-query (:for-update
                      (:select t :from 'cached-base-description
                               :where (:= 'entity-id entity)))
                     :single)
           (db-query (:update 'cached-base-description
                              :set 'description (calculate-base-description entity)
                              :where (:= 'entity-id entity))))
          (t
           (insert-row 'cached-base-description
                       :entity-id entity
                       :description (calculate-base-description entity)))))
  (when update-featured
    (map nil #'cache-base-description (db-query (:select 'entity-id
                                                         :from 'feature
                                                         :where (:= 'feature-id entity))
                                                :column))))

(defun indefinite-article (word)
  (if (find (first-elt (db-query (:select (:unaccent word)) :single))
            "aoeui" :test #'equal)
      "an"
      "a"))

(defun calculate-base-description (entity &key (include-features-p t))
  (when-let (result (db-query (:select 'n.singular
                                       ;; Pray for proper ordering. :(
                                       (:raw "array_agg(DISTINCT a.adjective)")
                                       (:raw "array_agg(DISTINCT f.feature_id)")
                                       :from (:as 'noun 'n)
                                       :left-join (:as 'adjective 'a)
                                       :on (:= 'a.entity-id 'n.entity-id)
                                       :left-join (:as 'feature 'f)
                                       :on (:= 'f.entity-id 'n.entity-id)
                                       :where (:= 'n.entity-id entity)
                                       :group-by 'n.singular)
                              :row))
    (destructuring-bind (noun adjectives features) result
      (with-output-to-string (s)
        (if-let (adjs (coerce (remove :null adjectives) 'list))
          (format s (concatenate 'string "~A " *english-list-format-string* " ")
                  (indefinite-article (car adjs)) adjs)
          (format s "~A " (indefinite-article noun)))
        (princ noun s)
        (when include-features-p
          (when-let (feature-descs (loop for feature across features
                                      for feature-desc = (unless (eq :null feature)
                                                           (calculate-base-description
                                                            feature
                                                            :include-features-p nil))
                                      when feature-desc
                                      collect feature-desc))
            (format s " with ")
            (format s *english-list-format-string* feature-descs)))))))

(defun base-description (entity)
  (values (db-query (:select 'description :from 'cached-base-description
                             :where (:= 'entity-id entity))
                    :single)))

;;;
;;; Nicknames
;;;
;;; - Nicknames are a system of altering the way an object is displayed to individual players.
;;;   These are associated at the entity-level, so:
;;;   1. Anyone who takes control of the entity will see (and be able to change) nicknames
;;;   associated with that character.
;;;   2. A player will have to create a nickname list for each character they control.
;;;
(defdao nickname ()
  ((entity-id bigint)
   (observer-id bigint)
   (nickname text)))

(defun nickname (observer entity)
  (db-query (:select 'nickname :from 'nickname
                     :where (:and (:= 'observer-id observer)
                                  (:= 'entity-id entity)))
            :single))

(defun (setf nickname) (new-value observer entity)
  (with-transaction ()
    (cond ((null new-value)
           (db-query (:delete-from 'nickname :where (:and
                                                     (:= 'observer-id observer)
                                                     (:= 'entity-id entity)))))
          ((nickname observer entity)
           (db-query (:update 'nickname :set 'nickname new-value
                              :where (:and
                                      (:= 'observer-id observer)
                                      (:= 'entity-id entity)))))
          (t
           (insert-row 'nickname
                       :entity-id entity
                       :observer-id observer
                       :nickname new-value))))
  new-value)

(defun all-nicknames (entity)
  (db-query (:select 'nickname 'observer-id :from 'nickname
                     :where (:= 'entity-id entity))
            :plists))

;;;
;;; Short Description
;;;
;;; - This is what is/should be used by the game as the basic display component for entities.
;;;
(defun short-description (observer entity)
  (values (db-query (:select (:as (:case ((:not-null 'n.id)
                                          'n.nickname)
                                    (t 'd.description))
                                  'short-description)
                             :from (:as 'cached-base-description 'd)
                             :left-join (:as 'nickname 'n)
                             :on (:and (:= 'n.entity-id 'd.entity-id)
                                       (:= 'n.observer-id observer))
                             :where (:= 'd.entity-id entity))
                    :single)))

(defun find-by-short-description (observer short-desc)
  (values (db-query (:select 'd.entity-id :from (:as 'cached-base-description 'd)
                             :inner-join (:as 'entity 'e)
                             :on (:= 'e.id 'd.entity-id)
                             :left-join (:as 'nickname 'n)
                             :on (:and (:= 'n.entity-id 'd.entity-id)
                                       (:= 'n.observer-id observer))
                             :where (:or
                                     (:ilike (:unaccent 'n.nickname)
                                             (:unaccent (format nil "%~A%" short-desc)))
                                     (:and
                                      (:is-null 'n.id)
                                      (:ilike (:unaccent 'd.description)
                                              (:unaccent (format nil "%~A%" short-desc))))))
                    :column)))

(defun reset ()
  (map nil #'rebuild-table '(noun adjective feature cached-base-description nickname)))
