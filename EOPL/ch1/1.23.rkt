#lang eopl

(define list-index
  (lambda (pred lst)
    (list-index-auxy pred lst 0)))

(define list-index-auxy
  (lambda (pred lst idx)
    (if (null? lst)
        #f
        (if (pred (car lst))
            idx
            (list-index-auxy pred (cdr lst) (+ 1 idx))))))

(provide list-index)
