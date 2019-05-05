#lang racket

(provide lex)

(define lex
  (lambda (expr ctx)
    (match expr
      [n #:when (number? n) `(const ,n)]
      [`(zero? ,expr1) `(zero? ,(lex expr1 ctx))]
      [`(sub1 ,expr1) `(sub1 ,(lex expr1 ctx))]
      [`(* ,expr1 ,expr2) `(* ,(lex expr1 ctx) ,(lex expr2 ctx))]
      [`(if ,pred-expr ,then-expr ,else-expr) `(if ,(lex pred-expr ctx) ,(lex then-expr ctx) ,(lex else-expr ctx))]
      [`(let ,bindings ,body)
        `(let
           ; Seeing from the test case, it assumes that let only allows single binding
           ,(car
              (map
                (lambda (binding)
                  (lex (cadr binding) ctx))
                bindings))
           ,(lex
              body
              (foldr
                (lambda (binding new-ctx)
                  (cons (car binding) new-ctx))
                ctx
                bindings)))]
      [`(lambda (,id) ,body)
        `(lambda ,(lex body (cons id ctx)))]
      [`(,exp1 ,exp2)
        `(,(lex exp1 ctx) ,(lex exp2 ctx))]
      [`,id
        (let ([idx (index-of ctx id)])
          (if idx (list 'var idx) id))])))
