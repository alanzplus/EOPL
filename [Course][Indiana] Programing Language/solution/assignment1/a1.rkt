#lang racket

(provide countdown)
(provide insertR)
(provide remv-1st)
(provide list-index-ofv?)
(provide filter)
(provide zip)
(provide map)
(provide append)
(provide reverse)
(provide fact)
(provide fib)
(provide binary->natural)
(provide minus)
(provide div)
(provide append-map)
(provide set-difference)
(provide powerset)
(provide cartesian-product)
(provide insertR-fr)
(provide filter-fr)
(provide map-fr)
(provide append-fr)
(provide reverse-fr)
(provide binary->natural-fr)
(provide append-map-fr)
(provide set-difference-fr)
(provide powerset-fr)
(provide cartesian-product-fr)

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

(define append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(null? lst2) lst1]
      [else
        (let helper
          ([lst lst1])
          (match lst
            [(list head tail ...)
             (cons head (helper tail))]
            ['() lst2]))])))

(define reverse
  (lambda (lst)
    (let helper
      ([lst lst]
       [res '()])
      (match lst
        ['() res]
        [(list head tail ...)
         (helper tail (cons head res))]))))

(define fact
  (lambda (n)
    (if (eqv? 0 n) 1 (* n (fact (- n 1))))))

(define fib
  (lambda (n)
    (cond
      [(eqv? n 0) 0]
      [(eqv? n 1) 1]
      [else (+ (fib (- n 1)) (fib (- n 2)))])))

(define binary->natural
  (lambda (lst)
    (let helper
      ([lst lst]
       [p 1]
       [res 0])
      (match lst
        ['() res]
        [(list head tail ...)
         (helper tail (* p 2) (+ res (* p head)))]))))

(define minus
  (lambda (n1 n2)
    (if (eqv? n2 0)
      n1
      (minus (- n1 1) (- n2 1)))))

(define div
  (lambda (n1 n2)
    (let helper
      ([n1 n1]
       [res 0])
      (if (eqv? n1 0)
        res
        (helper (- n1 n2) (+ 1 res))))))

(define append-map
  (lambda (f lst)
    (match lst
      ['() '()]
      [(list head tail ...)
       (append (f head) (append-map f tail))])))

(define set-difference
  (lambda (s1 s2)
    (filter (lambda (ele) (not (index-of s2 ele))) s1)))

(define powerset
  (lambda (lst)
    (match lst
      ['() (list '())]
      [(list head tail ...)
       (let [(res (powerset tail))]
         (append
           res
           (map (lambda (ele) (cons head ele)) res)))])))

(define cartesian-product
  (lambda (l1 l2)
    (match l1
      ['() '()]
      [(list head tail ...)
       (append
         (map (lambda (ele) (cons head (cons ele '()))) l2)
         (cartesian-product tail l2))])))

(define insertR-fr
  (lambda (s1 s2 lst)
    (foldr
      (lambda (ele res)
        (if (eqv? s1 ele)
          (cons s1 (cons s2 res))
          (cons ele res)))
      '() lst)))

(define filter-fr
  (lambda (pred lst)
    (foldr
      (lambda (ele res)
        (if (pred ele)
          (cons ele res)
          res))
      '() lst)))

(define map-fr
  (lambda (f lst)
    (foldr
      (lambda (ele res) (cons (f ele) res))
      '() lst)))
(define append-fr
  (lambda (l1 l2)
    (foldr
      (lambda (ele res) (cons ele res))
      l2 l1)))

(define reverse-fr
  (lambda (lst)
    (foldr
      (lambda (ele res) (append res (list ele)))
      '()
      lst)))

(define binary->natural-fr
  (lambda (lst)
    (foldr
      (lambda (ele res)
        (+ ele (* 2 res)))
      0
      lst)))

(define append-map-fr
  (lambda (f lst)
    (foldr
      (lambda (ele res)
        (append (f ele) res))
      '()
      lst)))

(define set-difference-fr
  (lambda (s1 s2)
    (foldr
      (lambda (ele res)
        (if (not (index-of s2 ele))
          (cons ele res)
          res))
      '()
      s1)))

(define powerset-fr
  (lambda (lst)
    (foldr
      (lambda (ele res)
        (append
          (map (lambda (e)
                 (cons ele e)) res)
          res))
      '(())
      lst)))

(define cartesian-product-fr
  (lambda (l1 l2)
    (foldr
      (lambda (ele res)
        (append
          (map (lambda (e) (cons ele (cons e '()))) l2)
          res))
      '()
      l1)))
