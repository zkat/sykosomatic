(cl:in-package #:belletrist)

(defvar *server* nil)
(defparameter *current-story* (list "Once upon a time..."))

(defun begin-shared-hallucination ()
  (when *server* (end-shared-hallucination) (print "Restarting..."))
  (start (setf *server* (make-instance 'acceptor :port 8888)))
  t)

(defun end-shared-hallucination ()
  (when *server* (stop *server*) (setf *server* nil)))

(define-easy-handler (home :uri "/") ()
  (unless *session*
    (start-session))
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Hello!")
      (<:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js" :type "text/javascript")
      (<:script :type "text/javascript" (<:ai "
function callback(data) {
  $('#chat-box').html(data);
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
  (push msg *current-story*)
  (update-chat))

(defajax update-chat ()
  (with-yaclml-output-to-string
    (mapc (lambda (message)
            (<:p (<:ai message)))
          (reverse *current-story*))))
