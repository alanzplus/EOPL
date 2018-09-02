#lang eopl

(define remove
  (lambda (s lst)
    (if (null? lst)
        '()
        (if (eqv? (car lst) s)
            (remove s (cdr lst))
            (cons
              (car lst)
              (remove s (cdr lst)))))))

(provide remove)
