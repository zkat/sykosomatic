(cl:defpackage #:sykosomatic.handler.newchar
  (:use :cl :hunchentoot
        :alexandria
        :yaclml
        :sykosomatic.handler
        :sykosomatic.character
        :sykosomatic.session
        :sykosomatic.account)
  (:export :newchar))
(cl:in-package #:sykosomatic.handler.newchar)

(defparameter *origins* '(("local" . "Local -- is from the Twin Cities area.")
                          ("state" . "Minnesotan -- not from the Cities, but still from the state.")
                          ("midwest" . "Midwestern -- hails from elsewhere in the American Midwest.")
                          ("east-coast" . "East Coast -- is from the east coast of the US.")
                          ("south" . "Southern -- comes from the Southern US.")
                          ("west-coast" . "West Coast -- California, Pacific Northwest, etc.")
                          ("else" . "Elsewhere -- Alaska, Hawaii, or other countries.")))

(defparameter *parents* '(("none" . "None")
                          ("one" . "One")
                          ("two" ."Two")
                          ("more" . "More than two")))

(defparameter *siblings* '(("none" . "None")
                           ("one" . "One")
                           ("two" . "Two")
                           ("three" . "Three")
                           ( "more" . "More than three")))

(defparameter *situations* '(("poor" . "Poor")
                             ("working-class" . "Working Class")
                             ("middle-class" . "Middle Class")
                             ("upper-class" . "Upper Class")))

(defparameter *careers* '(("lumberjack" . "Lumberjack")
                          ("programmer" . "Software Developer")
                          ("messiah" . "Savior")))

(defparameter *friends* '(("ronery" . "No, character is all alone.")
                          ("acquaintances" . "Not really, just some acquaintances/coworkers and such.")
                          ("tight" . "Yeah, but just one, or a couple of very close friends.")
                          ("social" . "Yeah, the character has plenty of friends, but few are really close.")
                          ("loved-by-everyone" . "Yes. The character has a relatively big circle of acquaintances and close friends.")))

(defparameter *so* '(("ronery" . "No, the character is forever alone.")
                     ("dating" . "Kinda, currently seeing someone.")
                     ("committed" . "Yes. The character has been with someone for a while.")
                     ("ball-and-chain" .
                      "Yes, the character is in a committed relationship and/or married.")))

(defparameter *location-descriptions* '(("midway" . "Midway Area")
                                        ("downtown" . "Downtown Minneapolis")
                                        ("dinkytown" . "Dinkytown Neighborhood")
                                        ("riverfront" . "Riverfront District")
                                        ("west-bank" . "West Bank Neighborhood")))

(defparameter *adjectives*
  (with-open-file (s (asdf:system-relative-pathname 'sykosomatic "features.txt"))
    (read s)))

(define-easy-handler (newchar :uri "/newchar") ()
  #+nil((careers :parameter-type 'array)
        (career-times :parameter-type 'array)
        (bodyparts :parameter-type 'array)
        (bodypart-adjs :parameter-type 'array))
  #+nil(ensure-logged-in)
  (with-form-errors
    (templ:render-template "newchar" :origins *origins*
                           :parents *parents*
                           :siblings *siblings*
                           :situations *situations*
                           :friends *friends*
                           :so *so*
                           :careers *careers*
                           :location-opts *location-descriptions*
                           :adjectives *adjectives*)))

(define-easy-handler (newchar-bodypart-adjs :uri "/newchar/bodypart-adjs") (adj)
  (when-let (opts (cdr (assoc adj *adjectives* :test #'string-equal)))
    (with-yaclml-output-to-string (templ:render-template "bodypart-adj-select" opts))))

(define-easy-handler (newchar-location-description :uri "/newchar/location-description") (loc)
  (let ((desc (cdr (assoc loc *location-descriptions* :test #'string-equal))))
    (when desc
      (setf (content-type*) "text/plain")
      desc)))
