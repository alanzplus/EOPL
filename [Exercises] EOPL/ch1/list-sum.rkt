#lang eopl

(define list-sum
  (lambda (lst)
    (if (null? lst)
        0
        (+
          (car lst)
          (list-sum (cdr lst))))))

(provide list-sum)
