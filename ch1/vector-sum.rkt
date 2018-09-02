#lang eopl

(define vector-sum
  (lambda (vec)
    (let ((n (vector-length vec)))
      (if (zero? n)
        0
        (partial-vector-sum vec (- n 1))))))

(define partial-vector-sum
  (lambda (vec idx)
    (if (zero? idx)
        (vector-ref vec idx)
        (+
          (vector-ref vec idx)
          (partial-vector-sum vec (- idx 1))))))

(provide vector-sum)
