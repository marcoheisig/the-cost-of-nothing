;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defun report (&key (stream *standard-output*))
  (format stream "~&~%= The Cost Of Nothing - System Report =~%")
  (format stream "Implementation: ~:[unknown~;~:*~a~]~@[ ~a~]~%"
          (lisp-implementation-type)
          (lisp-implementation-version))
  (format stream "Machine: ~:[unknown~;~:*~a~]~@[ ~a~]~%"
          (machine-type)
          (machine-version))
  (format stream "Hostname: ~a~%" (machine-instance))
  ;; TODO report benchmark results.
  (values))

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
