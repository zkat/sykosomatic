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
    (push "You must be logged in to access that page."
          (session-value 'errors))
    (redirect "/login")))

(defun active-account-sessions (account-name)
  "Finds all sessions that are logged in as ACCOUNT-NAME."
  (loop for (nil . session) in (session-db *server*)
     for session-user = (session-value 'account-name session)
     when (and session-user (string-equal session-user account-name))
     collect session))

;;;
;;; Handlers
;;;

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
         (push (format nil "You must select a character before playing.")
               (session-value 'errors))
         (redirect "/role"))
        (t (templ:stage char))))

(define-easy-handler (role :uri "/role") ()
  (ensure-logged-in)
  (templ:role (mapcar #'character-name
                      (find-characters-by-account-name (session-value 'account-name)))))

(define-easy-handler (scenes :uri "/scenes") ()
  (ensure-logged-in)
  (templ:scenes (mapcar #'scene-id (find-scenes-by-account-name (current-account-name)))))

(define-easy-handler (view-scene :uri "/view-scene") (id)
  (case (request-method*)
    (:get
     ;; TODO - validate scene id.
     (templ:view-scene id (session-value 'account-name) (scene-rating id)))
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
       (push (format nil "Already logged in as ~A." account-name)
             (session-value 'errors)))
     (templ:login))
    (:post
     (if-let ((account (validate-credentials account-name password)))
       (progn
         (setf (session-value 'account-name) (account-name account)
               (session-value 'display-name) (account-display-name account))
         (logit "~A logged in." account-name)
         (redirect "/role"))
       (progn
         (push "Invalid login or password." (session-value 'errors))
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
     (templ:signup))))

;;; Characters
(define-easy-handler (newchar :uri "/newchar") ((careers :parameter-type 'array)
                                                (career-times :parameter-type 'array)
                                                (bodyparts :parameter-type 'array)
                                                (bodypart-adjs :parameter-type 'array))
  #+nil(ensure-logged-in)
  (templ:newchar))

(define-easy-handler (newchar-career :uri "/newchar/career") ((idx :parameter-type 'integer))
  (when idx
    (with-yaclml-output-to-string (templ:career-div idx))))

(define-easy-handler (newchar-preview :uri "/newchar-preview") (pronoun first-name
                                                                nickname last-name
                                                                origin parents
                                                                siblings childhood-finances)
  ;; TODO - *all* of these need to be validated.
  (setf (content-type*) "text/plain")
  (templ:newchar-preview-div :first-name first-name
                             :nickname nickname
                             :last-name last-name
                             :pronoun pronoun
                             :pluralp (when (string-equal pronoun "they") t)
                             :origin origin
                             :parents parents
                             :siblings siblings
                             :class childhood-finances))

(define-easy-handler (newchar-careers :uri "/newchar-careers") ()
  (setf (content-type*) "application/json")
  (jsown:to-json (list "hairdresser")))

(define-easy-handler (newchar-bodyparts :uri "/newchar-bodyparts") ()
  (setf (content-type*) "application/json")
  (jsown:to-json (list "hair")))

(define-easy-handler (newchar-adjectives :uri "/newchar-adjectives") (bodypart)
  ;; (setf (content-type*) "application/json")
  (jsown:to-json (cond ((string= bodypart "hair")
                        '(:obj
                          ("colors" .
                           ("blonde" "black" "blue-black" "dark brown" "tan" "golden" "sandy" "scarlet"
                            "strawberry-blonde" "red" "dark red" "brown with blonde highlights"
                            "platinum" "orange" "ginger" "white" "grey" "black streaked with grey"
                            "brown streaked with grey" "dyed dark green" "dyed blue" "dyed rainbow"
                            "dyed red underneath" "dyed purple" "dyed turquise" "dyed lemon yellow"
                            "dyed fire red" "dyed lime green" "dyed sky blue" "dyed pink"))
                          ("textures" .
                           ("straight" "silky" "smooth" "greasy" "slick" "flowing" "wavy" "curly"
                            "frizzy" "kinky"))
                          ("thickness" . ("thick" "lush" "wispy" "thin" "thinning" "bald"))
                          ("length and style" .
                           ("toe-length" "waist-length" "long" "shoulder-length" "bobbed" "chin-length"
                            "afro" "short" "ponytail" "pigtails" "bun" "two buns" "mass of ringlets"
                            "buzzed" "shaved" "spiked" "feathered" "piled" "tangled"))
                          ("general" .
                           ("wild" "untamed" "mussed" "smelly" "crusted" "shredded" "vivid" "striking"
                            "classy" "mutinous" "gleaming" "unremarkable" "limp")))
))))
;;; Misc
(define-easy-handler (ajax-ping :uri "/pingme") ()
  (ensure-logged-in)
  (setf (content-type*) "text/plain")
  "pong")
