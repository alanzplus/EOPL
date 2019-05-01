#lang racket

(provide list-ref)
(provide union)
(provide extend)
(provide walk-symbol)
(provide lambda->lumbda)

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

(define extend
  (lambda (v pred)
    (lambda (x)
      (or (eqv? v x) (pred x)))))

(define walk-symbol
  (lambda (s lst)
    (let helper
      ([sym s]
       [l lst])
      (match l
        ['() sym]
        [(list head tail ...)
         (let
           [(t (car head))
            (v (cdr head))]
           (if (eqv? t sym)
             (if (symbol? v) (helper v lst) v)
             (helper sym tail)))]))))

(define (lambda->lumbda expr)
  (match expr
    [`(lambda (,id) ,body) `(lumda (,id) ,body)]
    [e e]))
