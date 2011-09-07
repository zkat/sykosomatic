(cl:in-package :sykosomatic)

(optimizations)

;;;
;;; HT
;;;
(defun init-hunchentoot ()
  (setf *dispatch-table*
        (list (create-folder-dispatcher-and-handler
               "/res/" *sykosomatic-path*)
              'dispatch-easy-handlers
              'default-dispatcher))
  (setf *default-handler* '404-handler)
  (pushnew 404 *approved-return-codes*)
  (setf *rewrite-for-session-urls* nil)
  (start (setf *server* (make-instance 'sykosomatic-acceptor
                                       :port *web-server-port*
                                       :request-class 'persistent-session-request)))
  (setf *catch-errors-p* nil))

(defun teardown-hunchentoot ()
  (when *server* (stop *server*) (setf *server* nil)))

;;;
;;; Handlers
;;;
(defmacro with-form-errors (&body body)
  `(let ((templ:*errors* (session-errors)))
     (unwind-protect (progn ,@body)
       (setf (session-errors) nil))))

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (templ:render-template "not-found"))

;;; Main page
(define-easy-handler (home :uri "/") ()
  (templ:render-template "home"))

(define-easy-handler (play :uri "/stage") (char)
  ;; TODO - make 'char' something other than the name. Some kind of external ID for the character.
  ;;        'find-character' is more useful for the parser, not as a unique identifier.
  (ensure-logged-in)
  (cond ((emptyp char)
         (push-error "You must select a character before playing.")
         (redirect "/role"))
        ((not (eql (character-account (find-character char))
                   (current-account)))
         (push-error "You're not authorized to play that character.")
         (redirect "/role"))
        (t (with-form-errors (templ:render-template "stage" char)))))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-db ()
    (with-form-errors
      (templ:render-template "role" (mapcar #'character-name
                                            (account-characters (current-account)))))))

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:render-template "scenes" (mapcar #'scene-id (find-scenes-by-account-id (current-account))))))

(define-easy-handler (view-scene :uri "/view-scene") ((id :parameter-type 'integer))
  (case (request-method*)
    (:get
     (cond ((scene-exists-p id)
            (with-form-errors
              (templ:render-template "view-scene" id (not (null (current-account))) (scene-rating id))))
           (t (push-error "No scene with ID ~A." id)
              (redirect "/scenes"))))
    (:post
     (ensure-logged-in)
     (cond ((account-voted-p id (current-account))
            (push-error "You've already voted for this scene.")
            (redirect "/view-scene"))
           (t
            (scene-upvote id (current-account))
            (redirect (format nil "/view-scene?id=~A" id)))))))

;;; Login/logout
(define-easy-handler (login :uri "/login") (email password)
  (case (request-method*)
    (:get
     (when-let ((account-id (current-account)))
       (push-error "Already logged in as ~A." (account-email account-id)))
     (with-form-errors (templ:render-template "login")))
    (:post
     (when (current-account)
       (redirect "/login"))
     (if-let ((account (validate-account email password)))
       (progn
         (start-persistent-session (sykosomatic.db:id account))
         (logit "~A logged in." email)
         (redirect "/role"))
       (progn
         (push-error "Invalid login or password.")
         (redirect "/login"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when *session*
    (logout *session*))
  (redirect "/login"))

;;; Account creation and management
(define-easy-handler (signup :uri "/signup") (email display-name password confirmation)
  (case (request-method*)
    (:post
     (multiple-value-bind (account-created-p errors)
         (create-account email display-name password confirmation)
       (if account-created-p
           (progn
             (logit "Account created: ~A" email)
             (push-error "Please log in.")
             (redirect "/login"))
           (progn
             (appendf (session-errors) errors)
             (redirect "/signup")))))
    (:get
     (with-form-errors
       (templ:render-template "signup")))))

;;; Characters
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

;;; Misc
(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in)
  (setf (content-type*) "text/plain")
  "pong")
