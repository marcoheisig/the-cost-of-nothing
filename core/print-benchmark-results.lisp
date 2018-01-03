;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/core/print-benchmark-results
  (:use :alexandria :closer-common-lisp)
  (:export
   #:define-benchmark
   #:query-benchmarks
   #:print-benchmark-results
   #:show-benchmark-results
   #:show-benchmark-results!))

(in-package :the-cost-of-nothing/core/print-benchmark-results)

(defvar *tags* nil
  "A list of symbols, tracking all existing benchmark tags.")

(defvar *benchmarks* nil
  "An alist. Each entry is a cons, whose car is a list of benchmark tags
  and whose cdr is the name of a benchmark function.")

(defvar *memoization-table* (make-hash-table :test #'eq)
  "A hash table, mapping from function names used by DEFINE-BENCHMARK to
  their result-reporting closures.")

(defmacro define-benchmark (name tags &body body)
  "Define a new benchmark with the given TAGS. BODY must return a function
  of one argument - a stream - that reports it findings to said stream."
  (assert (every #'symbolp tags))
  `(progn
     (loop for tag in ',tags do (pushnew tag *tags*))
     (pushnew (cons ',tags (defun ,name () ,@body))
              *benchmarks*
              :key #'cdr)))

(defun query-benchmarks (&rest tags)
  "Return a list of all benchmarks (by name) that share at least one tag
with the given list of symbols TAGS. If no TAGS are given, return a list of
all benchmarks."
  (assert (every #'symbolp tags))
  (if (null tags)
      (reverse (mapcar #'cdr *benchmarks*))
      (loop for (benchmark-tags . benchmark-name) in *benchmarks*
            when (intersection benchmark-tags tags)
              collect benchmark-name)))

(defun run-benchmark! (benchmark-name)
  (setf (gethash benchmark-name *memoization-table*)
        (funcall benchmark-name)))

(defun run-benchmark (benchmark-name)
  (or (gethash benchmark-name *memoization-table*)
      (run-benchmark! benchmark-name)))

(defun print-benchmark-results (&key tags stream force)
  (flet ((report (benchmark-name)
           (check-type benchmark-name symbol)
           (let ((report-fn
                   (if force
                       (run-benchmark! benchmark-name)
                       (run-benchmark benchmark-name))))
             (let ((*print-case* :capitalize))
               (format stream "~&~%= ~A =~%" benchmark-name))
             (funcall report-fn stream)
             (format stream "~&"))))
    (mapc #'report (apply #'query-benchmarks tags))
    (values)))

(defun show-benchmark-results! (&rest tags)
  "Rerun and report all benchmarks matching at least one of the given
TAGS. If no TAGS are given, select all benchmarks."
  (print-benchmark-results :stream *trace-output* :tags tags :force t))

(defun show-benchmark-results (&rest tags)
  "Report all benchmarks matching at least one of the given TAGS. If no
TAGS are given, select all benchmarks.

Benchmark results are memoized to speed up subsequent queries. If this is
not desired, use SHOW-BENCHMARK-RESULTS! instead."
  (print-benchmark-results :stream *trace-output* :tags tags :force nil))
