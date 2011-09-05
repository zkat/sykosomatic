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
  (setf *session-removal-hook* 'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port *web-server-port*)))
  (setf *catch-errors-p* nil))

(defun teardown-hunchentoot ()
  (when *server* (stop *server*) (setf *server* nil)))

(defun logout (session)
  (let ((account-id (session-value 'account-id session))
        (websocket-clients (session-websocket-clients session)))
    (when account-id
      (logit "~A logged out." (account-email account-id)))
    (when websocket-clients
      (mapc #'disconnect-client websocket-clients))))

(defun session-cleanup (session)
  (logit "Session timed out. Trying to log it out...")
  (logout session))

(defun ensure-logged-in ()
  (unless (and *session* (current-account))
    (push-error "You must be logged in to access that page.")
    (redirect "/login")))

(defun active-account-sessions (account-email)
  "Finds all sessions that are logged in as ACCOUNT-EMAIL."
  (loop for (nil . session) in (session-db *server*)
     for session-user = (session-value 'account-id session)
     when (and session-user (string-equal (account-email session-user)
                                          account-email))
     collect session))

(defun push-error (format-string &rest format-args)
  (push (apply #'format nil format-string format-args)
        (session-value 'errors)))

;;;
;;; Handlers
;;;
(defmacro with-form-errors (&body body)
  `(let ((templ:*errors* (session-value 'errors)))
     (unwind-protect (progn ,@body)
       (setf (session-value 'errors) nil))))

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (templ:not-found))

;;; Main page
(define-easy-handler (home :uri "/") ()
  (templ:home))

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
        (t (with-form-errors (templ:stage char)))))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-db ()
    (with-form-errors
      (templ:role (mapcar #'character-name
                          (account-characters (current-account)))))))

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:scenes (mapcar #'scene-id (find-scenes-by-account-id (current-account))))))

(define-easy-handler (view-scene :uri "/view-scene") ((id :parameter-type 'integer))
  (case (request-method*)
    (:get
     (cond ((scene-exists-p id)
            (with-form-errors
              (templ:view-scene id (not (null (current-account))) (scene-rating id))))
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
  (unless *session*
    (start-session))
  (case (request-method*)
    (:get
     (when-let ((account-id (current-account)))
       (push-error "Already logged in as ~A." (account-email account-id)))
     (with-form-errors (templ:login)))
    (:post
     (when (current-account)
       (redirect "/login"))
     (if-let ((account (validate-account email password)))
       (progn
         (setf (current-account) (sykosomatic.db:id account))
         (logit "~A logged in." email)
         (redirect "/role"))
       (progn
         (push-error "Invalid login or password.")
         (redirect "/login"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (current-account))
    (logout *session*)
    (remove-session *session*))
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
             (redirect "/login"))
           (progn
             (appendf (session-value 'errors) errors)
             (redirect "/signup")))))
    (:get
     (with-form-errors
       (templ:signup)))))

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
    (templ:newchar :origins *origins*
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
    (with-yaclml-output-to-string (templ:bodypart-adj-select opts))))

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
