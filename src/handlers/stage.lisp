(util:def-file-package #:sykosomatic.handler.stage
  (:use :hunchentoot
        :sykosomatic.account
        :sykosomatic.components.describable
        :sykosomatic.websocket
        :sykosomatic.session
        :sykosomatic.handler
        :sykosomatic.template)
  (:export :stage))

(defun gameplay-js-libs ()
  '("res/js/lib/swfobject.js"
    "res/js/lib/web_socket.js"
    "res/js/lib/json2.js"
    "res/js/client.js"))

(defun render (character-id)
  (render-page "stage.html" (list :char-name (short-description character-id character-id)
                                  :websocket-session-token (generate-websocket-token (session-string))
                                  :additional-js (loop for lib in (gameplay-js-libs)
                                                    collect (list :js-path lib)))
               :title "All the World's a Stage"))

(define-easy-handler (play :uri "/stage") ((char :parameter-type 'integer))
  (ensure-logged-in)
  (let ((character-id (when char (nth char (account-bodies (current-account))))))
    (cond ((null character-id)
           (push-error "You must select a character before playing.")
           (redirect "/role"))
          (t (render character-id)))))
