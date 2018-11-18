;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(declaim (notinline touch))
(defun touch (object)
  "Protect OBJECT from compiler optimization."
  object)

(defmacro bench (form &rest args &key max-samples min-sample-time timeout overhead)
  "Evaluate FORM multiple times and print the averaged execution time to
*TRACE-OUTPUT*.

   Examples:
   (bench nil) => 0.00 nanoseconds
   (bench (make-hash-table)) => 247.03 nanoseconds"
  (declare (ignore max-samples min-sample-time timeout overhead))
  `(print-time (benchmark ,form ,@args) *trace-output*))

(defmacro benchmark (form &rest args &key max-samples min-sample-time timeout overhead)
  "Execute BODY multiple times to accurately measure its execution time in
seconds. The returned values are literally the same as those from an
invocation of MEASURE-EXECUTION-TIME with suitable lambdas.

Examples:
 (benchmark (cons nil nil)) -> 3.3d-9 1.0 36995264
 (benchmark (gc :full t))   -> 0.031 0.9 90"
  (declare (ignore max-samples min-sample-time timeout overhead))
  (let ((iterations (gensym "ITERATIONS")))
    `(benchmark-thunk
      (lambda (,iterations)
        (loop repeat ,iterations do
          (touch ,form)))
      ,@args)))

(defvar *default-overhead* 0.0d0)

(defvar *default-min-sample-time* 0.05)

(defun benchmark-thunk (thunk &key (timeout 2.0)
                                (min-sample-time *default-min-sample-time*)
                                (max-samples (/ timeout min-sample-time))
                                (overhead *default-overhead*))
  (let ((tmax (local-time:timestamp+
               (local-time:now)
               (floor (* timeout 1.0e9)) :nsec))
        (samples '())
        (number-of-samples 0))
    (loop do (push (sample-execution-time-of-thunk thunk min-sample-time) samples)
             (incf number-of-samples)
          until (or (local-time:timestamp>= (local-time:now) tmax)
                    (and (not (null max-samples))
                         (>= number-of-samples max-samples))))
    (- (/ (reduce #'+ samples) number-of-samples)
       overhead)))

(setf *default-overhead* (benchmark nil :timeout 2.0 :overhead 0))
