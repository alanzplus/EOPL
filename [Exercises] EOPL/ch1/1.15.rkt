#lang eopl

(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons
          x
          (duple (- n 1) x)))))
    
(provide duple)
