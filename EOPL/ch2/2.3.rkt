#lang eopl

(define one
  (lambda () 1))

(define diff
  (lambda (l r)
    (- l r)))

(provide one)
(provide diff)

