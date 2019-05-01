#lang racket

(provide list-ref)

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (if (eqv? n 0)
             ls
             (cdr (nth-cdr (- n 1)))))))
      (car (nth-cdr n)))))
