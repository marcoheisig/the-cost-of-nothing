;;; © 2016-2017 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(declaim (notinline touch))
(defun touch (object)
  "Protect OBJECT from compiler optimization."
  (declare (ignore object))
  (values))

(defmacro bench (form)
  "Evaluate FORM multiple times and print the averaged execution time to
   *TRACE-OUTPUT*.

   Examples:
   (bench nil) => 0.00 nanoseconds
   (bench (make-hash-table)) => 247.03 nanoseconds"
  `(progn
     (princ (as-time (benchmark ,form)) *trace-output*)
     (values)))

(defmacro nbench (form)
  "Evaluate FORM multiple times and print the averaged execution time of
   all statements enclosed in BENCHMARK forms to *TRACE-OUTPUT*

   Example:
   (bench (make-array 100)) => 89.72 nanoseconds
   (nbench
     (progn
       (benchmark (list 5))
       (make-array 100)
       (benchmark (list 6)))) => 4.54 nanoseconds"
  `(progn
     (princ (as-time (nested-benchmark ,form)) *trace-output*)
     (values)))

(defmacro benchmark (form)
  "Execute FORM multiple times to accurately measure its execution time in
   seconds. The returned values are literally the same as those from an
   invocation of MEASURE-EXECUTION-TIME with suitable lambdas.

   Examples:
   (benchmark (cons nil nil)) -> 3.3d-9 1.0 36995264
   (benchmark (gc :full t))   -> 0.031 0.9 90"
  `(nested-benchmark (benchmark ,form)))

(defmacro nested-benchmark (&body body)
  "Execute BODY multiple times to accurately measure the execution time in
   seconds of all statements that appear inside of a BENCHMARK
   statement. The returned values are literally the same as those from an
   invocation of MEASURE-EXECUTION-TIME with suitable lambdas.

   Examples:
   (/ (nested-benchmark
        (loop for key across keys do
          (benchmark (gethash key table))))
      (length keys))
   -> 1.5527d-8"
  (with-gensyms (iterations)
    `(measure-execution-time
      (lambda (,iterations)
        (loop :repeat ,iterations :do
          (macrolet ((benchmark (form) `(touch ,form)))
            ,@body)))
      :overhead
      (lambda (,iterations)
        (loop :repeat ,iterations :do
          (macrolet ((benchmark (form)
                        (declare (ignore form))
                        `(touch nil)))
            ,@body))))))

(defun measure-execution-time-of-thunk (thunk)
  "Execute THUNK and return the execution time of THUNK in seconds as a
   double-float."
  (let* ((t0 (get-internal-run-time))
         (_  (funcall thunk))
         (t1 (get-internal-run-time)))
    (declare (ignore _))
    (coerce (/ (- t1 t0) internal-time-units-per-second)
            'double-float)))

(defun measure-execution-time (fun &key (overhead #'identity) (timeout 2.0))
  "The function FUN must invoke a certain operation N times for any given
   integer N. The function OVERHEAD should execute the same code, but without
   this operation. An attempt is made to benchmark no longer than TIMEOUT
   seconds.

   Returns three values:
   duration    - a double-float denoting the duration of the operation in seconds
   confidence  - a single-flot between 0.0 (garbage) and 1.0 (absolute confidence)
   iterations  - the number N of iterations used to determine the result"
  (let ((min-effective-samples 100)
        (min-sampletime        0.1)
        (invocation-growth     1.77))
    (gc) ; expensive, but crucial for reasonable results
    (loop
      :for iterations :of-type unsigned-byte := 3
        :then (floor (* iterations invocation-growth))
      :for benchtime :of-type double-float := 0d0
        :then (measure-execution-time-of-thunk
               (lambda ()
                 (funcall fun iterations)))
      :for sampletime :of-type double-float := 0d0
        :then (- benchtime
                 (measure-execution-time-of-thunk
                  (lambda ()
                    (funcall overhead iterations))))
      :for confidence :of-type single-float := 0.0
        :then (if (not (and (plusp benchtime)
                            (plusp sampletime)))
                  0.0
                  (let ((sample-confidence
                          (/ (* iterations (/ sampletime benchtime))
                             min-effective-samples))
                        (time-confidence
                          (/ sampletime min-sampletime)))
                    (coerce
                     (* (min 1.0 sample-confidence)
                        (min 1.0 time-confidence))
                     'single-float)))
      :until (or (> benchtime timeout)
                 (= confidence 1d0))
      :finally
         (return
           (values
            (/ (max sampletime 0d0) iterations)
            confidence
            iterations)))))
