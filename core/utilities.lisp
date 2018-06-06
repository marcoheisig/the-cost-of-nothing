;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(uiop:define-package :the-cost-of-nothing/core/utilities
  (:use :alexandria :closer-common-lisp)
  (:export
   #:write-si-unit
   #:quantity-string
   #:y-intersection-and-slope))

(in-package :the-cost-of-nothing/core/utilities)

(define-constant +si-prefix-alist+
    '(("yotta" . 1d+24)
      ("zetta" . 1d+21)
      ("exa"   . 1d+18)
      ("peta"  . 1d+15)
      ("tera"  . 1d+12)
      ("giga"  . 1d+09)
      ("mega"  . 1d+06)
      ("kilo"  . 1d+03)
      (""      . 1d+00)
      ("milli" . 1d-03)
      ("micro" . 1d-06)
      ("nano"  . 1d-09)
      ("pico"  . 1d-12)
      ("femto" . 1d-15)
      ("atto"  . 1d-18)
      ("zepto" . 1d-21)
      ("yocto" . 1d-24))
  :test #'equal)

(defun write-si-unit (quantity unit stream)
  "Write the float QUANTITY "
  (let ((quantity (coerce quantity 'double-float)))
    (destructuring-bind (prefix . factor)
        (or
         (rassoc-if
          (lambda (exponent) (> (/ quantity exponent) 1d0))
          +SI-prefix-alist+)
         '("" . 1d0))
      (format stream "~,2F ~A~A" (/ quantity factor) prefix unit))))

(defun quantity-string (quantity unit)
  (with-output-to-string (stream)
    (write-si-unit quantity unit stream)))

(defun y-intersection-and-slope (x0 y0 x1 y1)
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (slope
           (if (and (plusp dx) (plusp dy))
               (/ dy dx)
               0d0))
         (y-intersection
           (max 0d0 (- y0 (* slope x0)))))
    (values y-intersection slope)))

(defun bytes-consed ()
  "Return the number of bytes consed since some arbitrary, but fixed, time
in the past.  The secondary return value is a boolean, indicating whether
the returned information is meaningful."
  (values
   #+sbcl
   (sb-ext:get-bytes-consed)
   #+ecl
   (ffi:c-inline () () :object "ecl_make_unsigned_integer(GC_get_total_bytes())" :one-liner t)
   t)
  #-(or ecl sbcl)
  (values 0 nil))
