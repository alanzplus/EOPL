#lang eopl

(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

(define number-elements-from
  (lambda (lst idx)
    (if (null? lst)
        '()
        (cons
          (list (car lst) idx)
          (number-elements-from (cdr lst) (+ idx 1))))))

(provide number-elements)
