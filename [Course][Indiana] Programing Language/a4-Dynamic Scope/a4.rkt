#lang racket

(provide lex)
(provide value-of-fn)
(provide value-of-ds)
(provide closure-fn)
(provide closure-ds)
(provide apply-closure-fn)
(provide apply-closure-ds)
(provide empty-env)

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

(define value-of-fn
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (apply-env env var)]
      [b #:when (boolean? b) b]
      [`(lambda (,id) ,body) (closure-fn id body env)]
      [`(let ,bindings ,body)
        (value-of-fn
          body
          (foldr
            (lambda (binding aggregate-env)
              (let ([id (car binding)]
                    [val (value-of-fn (cadr binding) env)])
                (extend-env id val aggregate-env)))
            env
            bindings))]
      [`(sub1 ,expr1)
        (- (value-of-fn expr1 env) 1)]
      [`(* ,expr1 ,expr2)
        (* (value-of-fn expr1 env) (value-of-fn expr2 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (value-of-fn pred-expr env)
          (value-of-fn then-expr env)
          (value-of-fn else-expr env))]
      [`(zero? ,expr1)
        (zero? (value-of-fn expr1 env))]
      [`(,expr1 ,expr2)
        (apply-closure-fn (value-of-fn expr1 env) (value-of-fn expr2 env))])))

(define closure-fn
  (lambda (var body env)
    (lambda (arg)
      (value-of-fn body (extend-env var arg env)))))

(define apply-closure-fn
  (lambda (closure arg)
    (closure arg)))

(define empty-env
  (lambda ()
    (lambda (var)
      (error "No binding for ~s" var))))

(define extend-env
  (lambda (id val env)
    (lambda (var)
      (if (eqv? var id)
        val
        (apply-env env var)))))

(define apply-env
  (lambda (env var)
    (env var)))

(define value-of-ds
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (apply-env env var)]
      [b #:when (boolean? b) b]
      [`(lambda (,id) ,body) (closure-ds id body env)]
      [`(let ,bindings ,body)
        (value-of-ds
          body
          (foldr
            (lambda (binding aggregate-env)
              (let ([id (car binding)]
                    [val (value-of-ds (cadr binding) env)])
                (extend-env id val aggregate-env)))
            env
            bindings))]
      [`(sub1 ,expr1)
        (- (value-of-ds expr1 env) 1)]
      [`(* ,expr1 ,expr2)
        (* (value-of-ds expr1 env) (value-of-ds expr2 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (value-of-ds pred-expr env)
          (value-of-ds then-expr env)
          (value-of-ds else-expr env))]
      [`(zero? ,expr1)
        (zero? (value-of-ds expr1 env))]
      [`(,expr1 ,expr2)
        (apply-closure-ds (value-of-ds expr1 env) (value-of-ds expr2 env))])))

(define closure-ds
  (lambda (var body env)
    (list var body env)))

(define apply-closure-ds
  (lambda (closure arg)
    (match closure
      [(list var body env)
       (value-of-ds body (extend-env var arg env))])))
