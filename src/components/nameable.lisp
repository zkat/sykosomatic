(util:def-file-package #:sykosomatic.components.nameable
  (:use :sykosomatic.db
        :sykosomatic.entity)
  (:export :add-name :delete-name :base-name :full-name :find-by-full-name
           :recalculate-full-name :refresh-all-full-names))

(defun calculate-full-name (base-name use-article-p adjectives titles first-name suffix suffix-titles)
  (with-output-to-string (s)
    (when use-article-p (princ "a " s))
    (when adjectives (format s *english-list-format-string* (coerce adjectives 'list)) (princ " " s))
    (when titles (map nil (curry #'format s "~A ") titles))
    (when first-name (format s "~A " first-name))
    (princ base-name s)
    (when suffix (format s " ~A" suffix))
    (when suffix-titles (format s "~{, ~A~}" (coerce suffix-titles 'list))))  )

(defdao nameable ()
  ((entity-id bigint)
   (base-name text)
   (use-article-p boolean :col-default nil)
   (adjectives (or db-null text[]))
   (titles (or db-null text[]))
   (first-name (or db-null text))
   (suffix (or db-null text))
   (suffix-titles (or db-null text[]))
   (full-name text))
  (:unique entity-id))

(defun add-name (entity base-name &key
                 use-article-p adjectives titles
                 first-name suffix suffix-titles)
  (flet ((ensure-vector (maybe-vec)
           (if (vectorp maybe-vec)
               maybe-vec
               (coerce maybe-vec 'vector))))
    (insert-row 'nameable
                :entity-id entity
                :base-name base-name
                :use-article-p use-article-p
                :adjectives (if adjectives (ensure-vector adjectives) :null)
                :titles (if titles (ensure-vector titles) :null)
                :first-name (or first-name :null)
                :suffix (or suffix :null)
                :suffix-titles (if suffix-titles (ensure-vector suffix-titles) :null)
                :full-name (calculate-full-name
                            base-name use-article-p adjectives titles
                            first-name suffix suffix-titles))))

(defun delete-name (entity)
  (db-query (:delete-from 'nameable :where (:= 'entity-id entity))))

(defun base-name (entity)
  (db-query (:select 'base-name :from 'nameable :where (:= 'entity-id entity))
            :single))

(defun full-name (entity)
  (db-query (:select 'full-name :from 'nameable :where (:= 'entity-id entity))
            :single))

(defun find-by-full-name (name &key fuzzyp)
  (db-query (sql-compile `(:select 'entity-id :from 'nameable :where
                                   (,(if fuzzyp :ilike :=)
                                     'full-name
                                     ,(if fuzzyp
                                          (format nil "%~A%" name)
                                          name))))
            :single))

(defun recalculate-full-name (entity)
  (with-transaction ()
    (let ((full-name-args (db-query (:for-update
                                     (:select 'base-name 'use-article-p 'adjectives 'titles
                                              'first-name 'suffix 'suffix-titles
                                              :from 'nameable
                                              :where (:= 'entity-id entity)))
                                    :row)))
      (db-query (:update 'nameable :set
                         'full-name (apply #'calculate-full-name
                                           (substitute nil :null full-name-args)))))))

(defun refresh-all-full-names ()
  (with-transaction ()
    (doquery (:select 'entity-id :from 'nameable) (entity)
      (with-db (:reusep nil)
        (recalculate-full-name entity)))))
