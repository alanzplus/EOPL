#lang racket

(provide list-ref)
(provide union)
(provide extend)
(provide walk-symbol)
(provide lambda->lumbda)
(provide var-occurs?)
(provide vars)
(provide unique-vars)
(provide var-occurs-free?)

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
    [`(lambda (,id) ,body) `(lumbda (,id) ,(lambda->lumbda body))]
    [`(,exp1 ,exp2) `(,(lambda->lumbda exp1) ,(lambda->lumbda exp2))]
    [`,id id]))

(define var-occurs?
  (lambda (var expr)
    (match expr
      [`(lambda (,id) ,body)
        (var-occurs? var body)]
      [`(,exp1 ,exp2)
        (or (var-occurs? var exp1) (var-occurs? var exp2))]
      [`,id (eqv? id var)])))

(define (vars expr)
  (match expr
      [`(lambda (,id) ,body) (vars body)]
      [`(,exp1 ,exp2) (append (vars exp1) (vars exp2))]
      [`,id (list id)]))

(define (unique-vars expr)
  (match expr
      [`(lambda (,id) ,body) (unique-vars body)]
      [`(,exp1 ,exp2) (union (unique-vars exp1) (unique-vars exp2))]
      [`,id (list id)]))

(define var-occurs-free?
  (lambda (var expr)
    (match expr
      [`(lambda (,id) ,body)
        (and (not (eqv? id var)) (var-occurs-free? var body))]
      [`(,exp1 ,exp2)
        (or (var-occurs-free? var exp1) (var-occurs-free? var exp2))]
      [`,id (eqv? id var)])))
