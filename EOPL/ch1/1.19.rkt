#lang eopl

(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((zero? n) (cons x (list-set (cdr lst) (- n 1) x)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) x))))))
      
(provide list-set)
