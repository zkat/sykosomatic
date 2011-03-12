(cl:in-package #:belletrist)

(defvar *server* nil)
(defvar *ajax-processor* (make-instance 'ajax-processor :server-uri "/ajax"))
(defparameter *current-story* (list "Once upon a time..."))

(defun begin-shared-hallucination ()
  (when *server* (end-shared-hallucination) (print "Restarting..."))
  (start (setf *server* (make-instance 'acceptor :port 8888)))
  (setf *dispatch-table* (list 'dispatch-easy-handlers
                               (create-ajax-dispatcher *ajax-processor*)))
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
      (<:ai (generate-prologue *ajax-processor*))
      (<:script :type "text/javascript" (<:ai "
function callback(response) {
  var story = response.firstChild.firstChild.nodeValue;
  document.getElementById('chat-box').innerHTML = story;
};

function addMsg() {
  ajax_add_message(document.getElementById('what-user-said').value, callback);
};
")))
     (<:body
      (<:div :id "chat-box")
      (<:form :name "user-story" :action "javascript:addMsg()"
       (<:ah "What do?")
       (<:input :type "text" :id "what-user-said")
       (<:input :type "submit" :value "Send"))))))

(defun-ajax add-message (msg) (*ajax-processor*)
  (push (format nil "User sez: ~A" msg) *current-story*)
  (with-yaclml-output-to-string
    (<:ah
     (with-yaclml-output-to-string
       (mapc (lambda (line) (<:p (<:ai line)))
             (reverse *current-story*))))))
