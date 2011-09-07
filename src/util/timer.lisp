(in-package :sykosomatic.util)

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun time-difference (time-before)
  "Checks the difference between the internal-time provided and the current time.
Returns both the difference in time and the current-time used in the computation"
  (let* ((time-now (now))
         (difference (- time-now time-before)))
    (if (minusp difference)
        0                               ; just in case
        (values (- time-now time-before)
                time-now))))

(defstruct (timer (:constructor %make-timer))
  last-time
  (time-delta 0)
  times
  (next-target-time 0)
  fps-limit
  (cumulative-time 0.0))

(defun make-timer (fps-limit &key (max-times-stored 10))
  (%make-timer :fps-limit fps-limit :times (make-queue max-times-stored)))

(defgeneric max-times-stored (timer)
  (:method ((timer timer))
    (queue-length (timer-times timer))))

(defgeneric (setf max-times-stored) (new-value timer)
  (:method (new-value (timer timer))
    (setf (timer-times timer) (make-queue new-value))))

(defun timer-tick (timer)
  (when (timer-fps-limit timer)
    (limit-fps timer))
  (with-accessors ((times timer-times) (last-time timer-last-time)
                   (cumulative-time timer-cumulative-time)
                   (dt timer-time-delta))
      timer
    (let ((now (now)))
      (when last-time
        (let ((time-delta (- now last-time)))
          (setf dt time-delta)
          (unless (zerop time-delta)
            (when (= (queue-count times) (max-times-stored timer))
              (decf cumulative-time (dequeue times)))
            (enqueue time-delta times)
            (incf cumulative-time time-delta))))
      (setf last-time now))))

(defun limit-fps (timer)
  (when (timer-fps-limit timer)
    (let* ((now (now))
           (sleep-time (* 1.25(- (timer-next-target-time timer) now))))
      (setf (timer-next-target-time timer)
            (+ (/ (timer-fps-limit timer))
               now))
      (when (plusp sleep-time)
        (sleep (float sleep-time))))))

(defun fps (timer)
  (if (zerop (timer-cumulative-time timer))
      0
      (/ (queue-count (timer-times timer))
         (timer-cumulative-time timer))))
