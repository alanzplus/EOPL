#lang eopl

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
          (list
            (cadr (car lst))
            (car (car lst)))
          (invert (cdr lst))))))

(provide invert)
