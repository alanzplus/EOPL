#lang eopl

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
          (list (car lst))
          (down (cdr lst))))))

(provide down)
