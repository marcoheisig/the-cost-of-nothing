;;; © 2016-2018 Marco Heisig - licensed under GPLv3, see the file COPYING

(in-package :the-cost-of-nothing)

(define-memo-function cons-cost ()
  (benchmark
   (cons nil nil)))

(define-memo-function make-list-cost (size &key (initial-element nil))
  (benchmark
   (make-list size :initial-element initial-element)))

(define-memo-function make-sequence-cost (result-type length &key (initial-element nil))
  (let ((fast-make-sequence
          (compiled-lambda
           `(length initial-element)
           `(make-sequence ',result-type length :initial-element initial-element))))
    (benchmark
     (funcall fast-make-sequence length initial-element))))

(define-memo-function make-struct-cost (&key (number-of-slots 0))
  ;; TODO
  (values))

(define-memo-function make-instance-cost (&key (number-of-slots 0))
  ;; TODO
  (values)
  )

(define-memo-function gc-cost (&key full)
  (benchmark
   (trivial-garbage:gc :full full)))
