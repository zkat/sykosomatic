(cl:in-package :sykosomatic)

(declaim (optimize debug))

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
  (let ((account-name (session-value 'account-name session))
        (websocket-clients (session-websocket-clients session)))
    (when account-name
      (logit "~A logged out." account-name))
    (when websocket-clients
      (mapc #'disconnect-client websocket-clients))))

(defun session-cleanup (session)
  (logit "Session timed out. Trying to log it out...")
  (logout session))

(defun ensure-logged-in ()
  (unless (and *session* (session-value 'account-name))
    (push-error "You must be logged in to access that page.")
    (redirect "/login")))

(defun active-account-sessions (account-name)
  "Finds all sessions that are logged in as ACCOUNT-NAME."
  (loop for (nil . session) in (session-db *server*)
     for session-user = (session-value 'account-name session)
     when (and session-user (string-equal session-user account-name))
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

(defun current-account-name (&optional (*session* *session*))
  (session-value 'account-name))

(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  (templ:not-found))

;;; Main page
(define-easy-handler (home :uri "/") ()
  (templ:home))

(define-easy-handler (play :uri "/stage") (char)
  (ensure-logged-in)
  ;; TODO - Check authorization. If the current session can't play that
  ;; character, get the hell out of here asap.
  (cond ((emptyp char)
         (push-error "You must select a character before playing.")
         (redirect "/role"))
        (t (with-form-errors (templ:stage char)))))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (with-db ()
    (with-form-errors
      (templ:role (mapcar #'character-name
                          (account-characters (sykosomatic.db:id
                                               (find-account (session-value 'account-name)))))))))

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (with-form-errors
    (templ:scenes (mapcar #'scene-id (find-scenes-by-account-email (current-account-name))))))

(define-easy-handler (view-scene :uri "/view-scene") (id)
  (case (request-method*)
    (:get
     ;; TODO - validate scene id.
     (with-form-errors
       (templ:view-scene id (session-value 'account-name) (scene-rating id))))
    (:post
     ;; TODO - Don't allow voting if user has already voted.
     (ensure-logged-in)
     (scene-upvote id (session-value 'account-name))
     (redirect (format nil "/view-scene?id=~A" id)))))

;;; Login/logout
(define-easy-handler (login :uri "/login") (account-name password)
  (unless *session*
    (start-session))
  (case (request-method*)
    (:get
     (when-let ((account-name (session-value 'account-name)))
       (push-error "Already logged in as ~A." account-name))
     (with-form-errors (templ:login)))
    (:post
     (if-let ((account (validate-account account-name password)))
       (progn
         (setf (session-value 'account-name) (account-email account)
               (session-value 'display-name) (account-display-name account))
         (logit "~A logged in." account-name)
         (redirect "/role"))
       (progn
         (push-error "Invalid login or password.")
         (redirect "/login"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (session-value 'account-name))
    (logout *session*)
    (remove-session *session*))
  (redirect "/login"))

;;; Account creation and management
(define-easy-handler (signup :uri "/signup") (account-name display-name password confirmation)
  (case (request-method*)
    (:post
     (multiple-value-bind (account-created-p errors)
         (create-account account-name display-name password confirmation)
       (if account-created-p
           (progn
             (logit "Account created: ~A" account-name)
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

(defparameter *adjectives*
  (with-open-file (s (asdf:system-relative-pathname 'sykosomatic "features.txt"))
    (read s)))

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
