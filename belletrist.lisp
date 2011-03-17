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
;; * Add another input field, 'action'.
;; * When someone logs in, draw a screenplay-style scene heading.
;; * Render each entry as screenplay-style Action/Dialog for the user that spoke.

(defvar *server* nil)
(defparameter *current-story* nil)
(defvar *users* nil)
(defvar *max-message-id* 0)

(defstruct user-message id user timestamp message)

(defun add-user-message (user message &optional (timestamp (get-universal-time))
                         &aux (id (incf *max-message-id*)))
  (push (make-user-message :id id :user user :timestamp timestamp :message message)
        *current-story*)
  id)

(defun get-recent-messages (last-message-id)
  (member (1+ last-message-id) (reverse *current-story*) :key #'user-message-id))

(defun session-cleanup (session)
  (let ((username (session-value 'username session)))
    (when username
      (deletef *users* username :test #'string-equal))))

(defun begin-shared-hallucination ()
  (when *server* (end-shared-hallucination) (warn "Restarting server."))
  (setf *users* nil
        *session-removal-hook* #'session-cleanup)
  (start (setf *server* (make-instance 'acceptor :port 8888)))
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))

(define-easy-handler (login :uri "/login") (username)
  (unless *session*
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
                  (session-value 'last-message-id) *max-message-id*)
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
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript")
      (<:script :type "text/javascript" (<:ai "
function callback(data) {
  $('#chat-box').append(data);
};

function addMsg() {
  var val = $('#what-user-said').val();
  $('#what-user-said').val('');
  $.get('ajax', { 'f' : 'ADD-MESSAGE', 'msg' : val }, callback);
}

function updateChat() {
  $.get('ajax', { 'f' : 'UPDATE-CHAT' }, callback);
}

$(document).ready(updateChat);
setInterval(updateChat, 1000);
")))
     (<:body
      (<:div :id "chat-box")
      (<:form :name "user-story" :action "javascript:addMsg()"
       (<:ah "What do?")
       (<:input :type "textarea" :id "what-user-said")
       (<:input :type "submit" :value "Send"))))))

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

(defajax add-message (msg)
  (add-user-message (session-value 'username) msg)
  (update-chat))

(defajax update-chat ()
  (prog1
      (with-yaclml-output-to-string
        (mapc (lambda (message)
                (<:p (<:ai (user-message-message message) " ("(user-message-user message)")")))
              (get-recent-messages (session-value 'last-message-id))))
    (setf (session-value 'last-message-id) *max-message-id*)))
