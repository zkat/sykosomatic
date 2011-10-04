(test:def-test-package session
  (:use :hunchentoot
        :sykosomatic.db
        :sykosomatic.session))

;; TODO - a with-acceptor macro that will temporarily set up a hunchentoot web server to test with.
(test sykosomatic-acceptor
  (is (find-class 'sykosomatic-acceptor)))
(test persistent-session-request
  (is (find-class 'persistent-session-request)))
(test current-account)
(test session-string)
(test start-persistent-session)
(test persistent-session-gc)
(test end-session)
(test verify-persistent-session)
(test register-session-finalizer)
(test unregister-session-finalizer)
(test transient-session-value)
(test remove-transient-session-values)
(test push-error)
(test session-errors)
