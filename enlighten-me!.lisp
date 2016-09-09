;;; © 2016 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(defparameter *questions* ())

(defmacro answer (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *questions*)))

(defun enlighten-me! ()
  (dolist (question *questions*)
    (format t "~a~%" question)
    (funcall (symbol-function question))))

(answer |What is the cost of calling a FLET?|
  (print-time
   (runtime
     (lambda (invocations)
       (flet ((f (x y) (eq x y)))
         (dotimes (i invocations)
           (f "foo" 5)))))))

(declaim (notinline binary-defun-stub))
(defun binary-defun-stub (a b)
  (declare (ignore a b)))

(answer |What is the cost of calling a binary DEFUN?|
  (print-time
   (runtime
     (lambda (invocations)
       (dotimes (i invocations)
         (binary-defun-stub i i))))))

(answer |What is the cost of checking a single list element?|
  (print-time
   (runtime
     (let ((l (make-circular-list 2 :initial-element 42)))
       (lambda (invocations)
         (dotimes (i invocations)
           (when (/= (car l) 42) (return :?))
           (setf l (cdr l))))))))

