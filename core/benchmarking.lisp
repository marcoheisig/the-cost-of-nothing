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
  `(progn (print-time (benchmark ,form ,@args) *trace-output*)
          (values)))

(defmacro benchmark (form &rest args &key max-samples min-sample-time timeout overhead)
  "Execute BODY multiple times to accurately measure its execution time in
seconds. The returned values are literally the same as those from an
invocation of MEASURE-EXECUTION-TIME with suitable lambdas.

Examples:
 (benchmark (cons nil nil)) -> 3.3d-9 1.0 36995264
 (benchmark (gc :full t))   -> 0.031 0.9 90"
  (declare (ignore max-samples min-sample-time timeout overhead))
  `(benchmark-thunk (lambda () ,form) ,@args))

(defvar *default-overhead*)

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
    (- (/ (reduce #'+ samples)
          (coerce number-of-samples 'single-float))
       overhead)))

(defun sample-execution-time-of-thunk (thunk min-sample-time)
  "Measure the execution time of invoking THUNK more and more often, until
the execution time exceeds MIN-SAMPLE-TIME."
  (loop for iterations = 1 then (1+ (* iterations (1+ (random 4))))
        for execution-time = (max 0.0d0 (execution-time-of-thunk
                                         (lambda ()
                                           (loop repeat iterations do
                                             (funcall thunk)))))
        when (> execution-time min-sample-time)
          do (return (/ execution-time iterations))))

(defun execution-time-of-thunk (thunk)
  "Execute THUNK and return the execution time of THUNK in seconds as a
double-float."
  (let* ((t0 (local-time:now))
         (_  (funcall thunk))
         (t1 (local-time:now)))
    (declare (ignore _))
    (max 0.0d0 (local-time:timestamp-difference t1 t0))))

(defvar *default-overhead* (benchmark nil :timeout 2.0 :overhead 0.0))
