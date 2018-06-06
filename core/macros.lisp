;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/core/macros
  (:use
   :alexandria
   :closer-common-lisp
   :the-cost-of-nothing/core/utilities
   :the-cost-of-nothing/core/measure-execution-time)
  (:export
   #:bench
   #:nbench
   #:benchmark
   #:nested-benchmark))

(in-package :the-cost-of-nothing/core/macros)

(defmacro bench (form &rest args)
  "Evaluate FORM multiple times and print the averaged execution time to
*TRACE-OUTPUT*.

   Examples:
   (bench nil) => 0.00 nanoseconds
   (bench (make-hash-table)) => 247.03 nanoseconds"
  `(progn
     (write-si-unit (benchmark ,form ,@args) "seconds" *trace-output*)
     (values)))

(defmacro nbench (form &rest args)
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
     (write-si-unit (nested-benchmark ,form ,@args) "seconds" *trace-output*)
     (values)))

(defmacro benchmark (form &rest args)
  "Execute FORM multiple times to accurately measure its execution time in
seconds. The returned values are literally the same as those from an
invocation of MEASURE-EXECUTION-TIME with suitable lambdas.

Examples:
 (benchmark (cons nil nil)) -> 3.3f-9 1.0 36995264
 (benchmark (gc :full t))   -> 0.031 0.9 90"
  `(nested-benchmark (benchmark ,form ,@args)))

(defmacro nested-benchmark (form &rest args)
  "Execute FORM multiple times to accurately measure the execution time in
seconds of all statements that appear within a BENCHMARK statement. The
returned values are literally the same as those from an invocation of
MEASURE-EXECUTION-TIME with suitable lambdas.

Examples:
 (/ (nested-benchmark
      (loop for key across keys do
        (benchmark (gethash key table))))
    (length keys))
 -> 1.5527f-8"
  (with-gensyms (iterations)
    `(measure-execution-time
      (lambda (,iterations)
        (loop repeat ,iterations do
          (macrolet ((benchmark (form) `(touch ,form))
                     (bench (form) `(touch ,form)))
            ,form)))
      :overhead
      (lambda (,iterations)
        (loop repeat ,iterations do
          (macrolet ((benchmark (form) (declare (ignore form)) `(touch nil))
                     (bench (form) (declare (ignore form)) `(touch nil)))
            ,form)))
      ,@args)))
