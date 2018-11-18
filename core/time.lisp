;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defvar *timestamp-fuzziness* 0
  "A timestamp fuzziness of zero means that each call to MAKE-TIMESTAMP
produces a new timestamp.  Values bigger than zero tell MAKE-TIMESTAMP to
return the same timestamp for the given number of consecutive calls.

Binding this variable to a positive value can be useful for profiling
performance sensitive regions of code, where the cost of measuring the time
would otherwise dominate the execution.")

(let ((timestamp-counter most-positive-fixnum)
      (timestamp-cache (local-time:now)))
  (defun make-timestamp ()
    "Return a local-time:timestamp representing the current time.  See also
the special variable *TIMESTAMP-FUZZINESS*."
    (if (>= timestamp-counter *timestamp-fuzziness*)
        (let ((timestamp (local-time:now)))
          (setf timestamp-counter 0)
          (setf timestamp-cache timestamp)
          timestamp)
        (progn (incf timestamp-counter)
               timestamp-cache))))

(defun execution-time-of-thunk (thunk)
  "Execute THUNK and return the execution time of THUNK in seconds as a
double-float."
  (let* ((t0 (local-time:now))
         (_  (funcall thunk))
         (t1 (local-time:now)))
    (declare (ignore _))
    (local-time:timestamp-difference t1 t0)))

(defun sample-execution-time-of-thunk (thunk min-sample-time)
  (loop for iterations = 1 then (1+ (* iterations (1+ (random 4))))
        for execution-time = (max 0.0d0 (execution-time-of-thunk
                                         (lambda ()
                                           (funcall thunk iterations))))
        when (> execution-time min-sample-time)
          do (return (/ execution-time iterations))))
