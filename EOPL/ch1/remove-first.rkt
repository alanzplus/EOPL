#lang eopl

(define remove-first
  (lambda (s lst)
    (if (null? lst)
        '()
        (if (eqv? s (car lst))
            (cdr lst)
            (cons
              (car lst)
              (remove-first s (cdr lst)))))))

(provide remove-first) 
