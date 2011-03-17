(cl:in-package #:belletrist)

;; Ideas:

;; Story:
;; In general, players can hang around and play casually however they want. If/when they have an
;; idea or want to play out something more seriously, they can choose to start a 'scene' to earn
;; points.

;; A scene will be recorded, and transcribed to look like a screenplay once it's done. It will then
;; be posted in a community area, where others can rate the overall scene, as well as individual
;; characters in it.

;; Scene scores are divided amongst players, and individual achievement scores give individual
;; players bonuses. Points are awarded based on score, and the points can be used towards something.

;; Templating:

;; All templating systems suck. This one won't. No logic goes into a template, and templates should
;; be kept small (like functions). Additionally, multiple templates will be kept in a single file,
;; which will generate either lisp functions or CLOS objects to correspond to each individual
;; template, which the programmer can compose with the logic (much like pages are strung together
;; with logic). The template files will basically look like lisp files, and require parameter
;; declaration for clarity, as well as accept an optional docstring.
;;
;; Example:
;; (deftempl standard-page (title head-contents body-contents)
;;   "This template renders the standard page thingy."
;;   <html>
;;     <head>
;;       <title>{title}</title>
;;       {head}
;;     </head>
;;     <body>
;;     {body-contents}
;;     </body>
;;   </html>)
;;
;; The above can be loaded (probably with a special reader macro), and could then create a function
;; to be called on a stream, with the required parameters:
;; (load "page.templ")
;; (render-template 'standard-page *standard-output* :title "My special page" :body-contents "<p>Hello, World!</p>")
;;
;; Question: Do even minor instances of HTML need to be templated out?

;; TODO for today:
;; * grey background for slug lines.
;; * group actions and dialog by user.
;; * Use (CONT'D.)
;; * Fix formatting of submission form.
(defvar *server* nil)
(defparameter *current-story* nil)
(defvar *users* nil)
(defvar *max-action-id* 0)

(defstruct user-action id user timestamp action dialogue)

(defun add-user-action (user action dialogue &optional (timestamp (get-universal-time))
                         &aux (id (incf *max-action-id*)))
  (push (make-user-action :id id :user user :timestamp timestamp :action action :dialogue dialogue)
        *current-story*)
  id)

(defun get-recent-actions (last-action-id)
  (member (1+ last-action-id) (reverse *current-story*) :key #'user-action-id))

(defun session-cleanup (session)
  (let ((username (session-value 'username session)))
    (when username
      (logout username))))

(defun logout (username)
  (deletef *users* username :test #'string-equal))

(defun begin-shared-hallucination ()
  (when *server* (end-shared-hallucination) (warn "Restarting server."))
  (setf *users* nil
        *session-removal-hook* #'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port 8888)))
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))

(define-easy-handler (login :uri "/login") (username)
  (if (and *session* (session-value 'username))
      (redirect "/")
      (start-session))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Login Page")
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript"))
     (<:body
      (if (and username (not (find username *users* :test #'string-equal)))
          (progn
            (<:p (<:ah "Successfully logged in as " username "."))
            (push username *users*)
            (setf (session-value 'username) username
                  (session-value 'last-action-id) *max-action-id*)
            (format t "~&~A logged in.~%" username)
            (redirect "/"))
          (<:div
           (<:form :name "username" :action "/login"
            (<:label (<:ah "Pick a username: "))
            (<:input :type "text" :name "username")
            (<:input :type "submit" :value "Submit"))
           (when username
             (<:label (<:ah "Sorry, that username is already being used.")))))))))

(define-easy-handler (home :uri "/") ()
  (unless (and *session* (session-value 'username))
    (redirect "/login"))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Hello!")
      (<:style :type "text/css" "

/* From: http://scrippets.org/ */
.chat-box {
    list-style: none;
    width: 420px;
    height: 500px;
    background: #fffffc;
    color: #000000;
    border: 1px solid #333;
    padding: 5px 14px 15px 14px !important;
    margin-left: auto;
    margin-right: auto;
    overflow:auto;
}

.chat-box p { font: 14px Courier, monospace, fixed !important;
              text-align: left !important;
              letter-spacing: 0 !important;
              margin-top: 0px !important;
              margin-bottom: 0px !important; }

.sceneheader, .action, .character { padding-top: 1.5ex; }

.character, .sceneheader { text-transform:uppercase; }

.action { padding-right: 5%; font-size: 12px !important; line-height: 14px !important; }

.character { margin-left: 40%; }

.dialogue { margin-left: 20%; padding-right: 20%; }

.parenthetical { margin-left: 32%; padding-right: 30%; }

/* special case: dialogue followed by a parenthetical; the extra line needs to be suppressed */

.dialogue + .parenthetical { padding-bottom: 0; }

.transition { padding-top: 3ex; margin-left: 65%; padding-bottom: 1.5ex; }

.user-story { margin-right: auto;
            margin-left: auto;
            width: 600px;
          }
.logout-button { margin-right: auto; margin-left: auto; width: 80px;}

"
)
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript")
      (<:script :type "text/javascript" (<:ai "
function callback(data) {
  $('#chat-box').append(data);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  $('#user-action').val('');
  $('#user-dialogue').val('');
  $.get('ajax', { 'f' : 'ADD-ACTION', 'action' : action, 'dialogue' : dialogue }, callback);
}

function updateChat() {
  $.get('ajax', { 'f' : 'UPDATE-CHAT' }, callback);
}

$(document).ready(updateChat);
setInterval(updateChat, 1000);
")))
     (<:body
      (<:div :class "chat-box" :id "chat-box" (<:p :class "scene-header" (<:ah "EXT. JOSH'S COMPUTER. NIGHT.")))
      (<:form :class "user-story" :name "user-story" :action "javascript:addMsg()"
       (<:label (<:ah "Action: "))
       (<:input :type "textarea" :id "user-action")
       (<:label (<:ah "Dialogue: "))
       (<:input :type "textarea" :id "user-dialogue")
       (<:input :type "submit" :value "Send"))
      (<:form :class "logout-button" :action "/logout"
        (<:input :type "submit" :value "Log Out"))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (when (and *session* (session-value 'username))
    (let ((username (session-value 'username)))
      (logout username)
      (setf (session-value 'username) nil)))
  (redirect "/login"))

(define-easy-handler (ajax :uri "/ajax") (f)
  (let ((func (find-ajax-func f)))
    (if func
        (progn
          (setf (content-type*) "text/html")
          (no-cache)
          (apply func (remove f (mapcar #'cdr (get-parameters*)))))
        (warn "An attempt was made to call undefined AJAX function ~A." f))))

(defparameter *ajax-funcs* (make-hash-table :test #'equalp))

(defun find-ajax-func (name)
  (gethash name *ajax-funcs*))

(defmacro defajax (name lambda-list &body body)
  `(progn
     (when (gethash ,(string name) *ajax-funcs*)
       (warn "Redefining AJAX function ~A." ,(string name)))
     (setf (gethash ,(string name) *ajax-funcs*)
           (defun ,name ,lambda-list ,@body))))

(defajax add-action (action dialogue)
  (add-user-action (session-value 'username) action dialogue)
  (update-chat))

(defun render-user-action (user-action)
  (let ((action (user-action-action user-action))
        (dialogue (user-action-dialogue user-action)))
    (<:div :class "user-entry"
      (if (and (not (emptyp action)) (emptyp dialogue))
          (<:p :class "action"
               (<:ah action))
          (progn
            (<:p :class "character"
                 (<:ah (user-action-user user-action)))
            (unless (emptyp action)
              (<:p :class "parenthetical"
                   (<:ah "(" action ")")))
            (<:p :class "dialogue"
                 (<:ah
                  (if (emptyp dialogue)
                      "..."
                      dialogue))))))))

(defajax update-chat ()
  (prog1 (with-yaclml-output-to-string
           (mapc #'render-user-action
                 (get-recent-actions (session-value 'last-action-id))))
    (setf (session-value 'last-action-id) *max-action-id*)))
