(in-package :the-cost-of-nothing)

(declaim (function *timestamp-function*))
(defvar *timestamp-function* #'local-time:now)

(declaim (inline make-timestamp))
(defun make-timestamp ()
  (funcall *timestamp-function*))

(defmacro with-fuzzy-timestamps ((&key (fuzziness '8)) &body body)
  "Set the timestamp fuzziness for the dynamic extent of BODY.  This can be
useful for profiling performance critical regions of code, where the cost
of measuring the time would otherwise dominate the execution.

A timestamp fuzziness of zero means that each call to MAKE-TIMESTAMP
produces a new timestamp.  Values bigger than zero mean that MAKE-TIMESTAMP
returns the same timestamp for the given number of consecutive calls."
  `(call-with-fuzzy-timestamps
    (lambda () ,@body)
    (the (integer 0 *) ,fuzziness)))

(defun call-with-fuzzy-timestamps (thunk fuzziness)
  (declare (type (integer 0 *) fuzziness))
  (let ((cache nil)
        (counter fuzziness))
    (let ((*timestamp-function*
            (lambda ()
              (cond ((>= counter fuzziness)
                     (setf counter 0)
                     (setf cache (local-time:now)))
                    (t
                     (incf counter)
                     cache)))))
      (funcall thunk))))
