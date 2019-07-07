#lang racket

(provide last-non-zero)
(provide lex)

(define last-non-zero
  (lambda (ls)
    (let/cc k
            (letrec
              ([helper
                 (lambda (ls)
                   (cond
                     [(null? ls) '()]
                     [(eq? (car ls) 0)
                      (k (helper (cdr ls)))]
                     [else
                       (cons (car ls) (helper (cdr ls)))]))])
              (helper ls)))))

; Assume throw has the form: "(throw exp1 exp2)"
(define lex
  (lambda (expr ctx)
    (match expr
           [n #:when (number? n) `(const ,n)]
           [`(zero? ,expr1) `(zero ,(lex expr1 ctx))]
           [`(sub1 ,expr1) `(sub1 ,(lex expr1 ctx))]
           [`(* ,expr1 ,expr2) `(mult ,(lex expr1 ctx) ,(lex expr2 ctx))]
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
             `(app ,(lex exp1 ctx) ,(lex exp2 ctx))]
           [`(let/cc ,k ,body)
             `(letcc ,(lex body (cons k ctx)))]
           [`(throw ,exp1 ,exp2)
             `(throw ,(lex exp1 ctx) ,(lex exp2 ctx))]
           [`,id
             (let ([idx (index-of ctx id)])
               (if idx (list 'var idx) id))]
           )))
