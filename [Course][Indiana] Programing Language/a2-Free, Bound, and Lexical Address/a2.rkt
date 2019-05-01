#lang racket

(provide list-ref)
(provide union)

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (if (eqv? n 0)
             ls
             (cdr (nth-cdr (- n 1)))))))
      (car (nth-cdr n)))))

(define union
  (lambda (l1 l2)
    (foldr
      (lambda (e res)
        (if (memv e res)
          res
          (cons e res)))
      l1
      l2)))
