#lang racket

(require racket/format)

(provide list-ref)
(provide union)
(provide extend)
(provide walk-symbol)
(provide lambda->lumbda)
(provide var-occurs?)
(provide vars)
(provide unique-vars)
(provide var-occurs-free?)
(provide var-occurs-bound?)
(provide unique-free-vars)
(provide unique-bound-vars)
(provide lex)
(provide walk-symbol-update)
(provide var-occurs-both?)

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

(define var-occurs-bound?
  (lambda (var expr)
    (let helper
      [(expr expr)
       (formal #f)]
      (match expr
        [`(lambda (,id) ,body)
          (if (eqv? var id)
            (helper body #t)
            (helper body #f))]
        [`(,exp1 ,exp2)
          (or (helper exp1 formal) (helper exp2 formal))]
        [`,id (and (eqv? id var) formal)]))))

(define (unique-free-vars expr)
  (let helper
    [(expr expr)
     (fps '())]
    (match expr
      [`(lambda (,id) ,body)
        (helper body (cons id fps))]
      [`(,exp1 ,exp2)
        (union (helper exp1 fps) (helper exp2 fps))]
      [`,id
        (if (not (memv id fps))
          (list id)
          '())])))

(define (unique-bound-vars expr)
  (let helper
    [(expr expr)
     (fps '())]
    (match expr
      [`(lambda (,id) ,body)
        (helper body (cons id fps))]
      [`(,exp1 ,exp2)
        (union (helper exp1 fps) (helper exp2 fps))]
      [`,id
        (if (memv id fps)
          (list id)
          '())])))

(define lex
  (lambda (expr ctx)
    (match expr
      [`(lambda (,id) ,body)
        `(lambda ,(lex body (cons id ctx)))]
      [`(,exp1 ,exp2)
        `(,(lex exp1 ctx) ,(lex exp2 ctx))]
      [`,id
        (let ([idx (index-of ctx id)])
          (if idx (list 'var idx) id))])))

(define walk-symbol-update
  (lambda (s lst)
    (define update-boxes
      (lambda (val boxes)
        (foldr
          (lambda (b v)
            (set-box! b v)
            v)
          val
          boxes)))
    (let helper
      ([s s]
       [l lst]
       [boxes '()])
      (match l
        ['() (update-boxes s boxes)]
        [(list head tail ...)
         (let
           ([t (car head)]
            [b (cdr head)]
            [v (unbox (cdr head))])
           (if (eqv? t s)
             (if (symbol? v)
               (helper v lst (cons b boxes))
               (update-boxes v (cons b boxes)))
             (helper s tail boxes)))]))))

(define var-occurs-both?
  (lambda (s expr)
    (let helper
      ([expr expr]
       [bound? #f])
      (match expr
        [`(lambda (,id) ,body)
          (if (eqv? id s)
            (helper body #t)
            (helper body #f))]
        [`(,exp1 ,exp2)
          (let-values
            ([(free1 bound1) (helper exp1 bound?)]
             [(free2 bound2) (helper exp2 bound?)])
            (values
              (or free1 free2)
              (or bound1 bound2)))]
        [`,id
          (values
            (and (not bound?) (eqv? s id))
            (and bound? (eqv? s id)))]))))
