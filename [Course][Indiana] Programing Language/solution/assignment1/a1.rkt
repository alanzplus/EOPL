#lang racket

(provide countdown)
(provide insertR)
(provide remv-1st)
(provide list-index-ofv?)
(provide filter)
(provide zip)
(provide map)

(define countdown
  (lambda (n)
    (if (eqv? n 0) (list 0)
      (cons n (countdown (- n 1))))))

(define insertR
  (lambda (s1 s2 lst)
    (match lst
      [(list head tail ...) 
       (if (eqv? head s1)
         (append (list s1 s2) (insertR s1 s2 tail))
         (cons head (insertR s1 s2 tail)))]
      ['() '()])))

(define remv-1st
  (lambda (s1 lst)
    (match lst
      [(list head tail ...)
       (if (eqv? head s1)
         tail
         (cons head (remv-1st s1 tail)))]
      ['() '()])))

(define list-index-ofv?
  (lambda (s lst)
    (let helper
      ([lst lst]
       [idx 0])
      (match lst
        [(list head tail ...)
         (if (eqv? head s)
           idx
           (helper tail (+ 1 idx)))]))))

(define filter
  (lambda (pred lst)
    (match lst
      ['() '()]
      [(list head tail ...)
       (if (pred head)
         (cons head (filter pred tail))
         (filter pred tail))])))

(define zip
  (lambda (lst1 lst2)
    (match* (lst1 lst2)
      [([list h1 t1 ...] [list h2 t2 ...]) (cons (cons h1 h2) (zip t1 t2))]
      [(_ _) '()])))

(define map
  (lambda (f lst)
    (match lst
      [(list head tail ...)
       (cons (f head) (map f tail))]
      ['() '()])))