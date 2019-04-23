#lang eopl

(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and
          (pred (car lst))
          (every? pred (cdr lst))))))

(provide every?)
