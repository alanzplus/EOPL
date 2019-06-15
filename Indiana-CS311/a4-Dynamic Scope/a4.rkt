#lang racket

(provide lex)
(provide value-of-fn)
(provide value-of-ds)
(provide closure-fn)
(provide closure-ds)
(provide apply-closure-fn)
(provide apply-closure-ds)
(provide value-of-dynamic)
(provide value-of-ri)
(provide empty-env)
(provide empty-env-fn)
(provide empty-env-ds)
(provide extend-env-fn)
(provide extend-env-ds)
(provide apply-env-fn)
(provide apply-env-ds)
(provide closure-fn-ri)
(provide apply-closure-fn-ri)
(provide closure-ds-ri)
(provide apply-closure-ds-ri)

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

(define value-of-dynamic
  (lambda (expr env)
    (match expr
      [`(quote ,v) v]
      [num #:when (number? num) num]
      [var #:when (symbol? var) (apply-env env var)]
      [b #:when (boolean? b) b]
      [`(lambda (,var) ,body)
        (lambda (arg env)
          (value-of-dynamic body (extend-env var arg env)))]
      [`(let ,bindings ,body)
        (value-of-dynamic
          body (foldr
                 (lambda (binding aggregate-env)
                   (let ([id (car binding)]
                         [val (value-of-dynamic (cadr binding) env)])
                     (extend-env id val aggregate-env)))
                 env
                 bindings))]
      [`(sub1 ,expr1)
        (- (value-of-dynamic expr1 env) 1)]
      [`(* ,expr1 ,expr2)
        (* (value-of-dynamic expr1 env) (value-of-dynamic expr2 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (value-of-dynamic pred-expr env)
          (value-of-dynamic then-expr env)
          (value-of-dynamic else-expr env))]
      [`(zero? ,expr1)
        (zero? (value-of-dynamic expr1 env))]
      [`(null? ,expr1)
        (null? (value-of-dynamic expr1 env))]
      [`(cons ,expr1 ,expr2)
        (cons (value-of-dynamic expr1 env) (value-of-dynamic expr2 env))]
      [`(car ,expr1)
        (car (value-of-dynamic expr1 env))]
      [`(cdr ,expr1)
        (cdr (value-of-dynamic expr1 env))]
      [`(,expr1 ,expr2)
        ((value-of-dynamic expr1 env) (value-of-dynamic expr2 env) env)])))

(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (lambda (expr)
      (let value-of
        ([expr expr]
         [env (empty-env)])
        (match expr
          [num #:when (number? num) num]
          [var #:when (symbol? var) (apply-env env var)]
          [b #:when (boolean? b) b]
          [`(lambda (,id) ,body) (closure id body env extend-env)]
          [`(let ,bindings ,body)
            (value-of
              body
              (foldr
                (lambda (binding aggregate-env)
                  (let ([id (car binding)]
                        [val (value-of (cadr binding) env)])
                    (extend-env id val aggregate-env)))
                env
                bindings))]
          [`(sub1 ,expr1)
            (- (value-of expr1 env) 1)]
          [`(* ,expr1 ,expr2)
            (* (value-of expr1 env) (value-of expr2 env))]
          [`(if ,pred-expr ,then-expr ,else-expr)
            (if (value-of pred-expr env)
              (value-of then-expr env)
              (value-of else-expr env))]
          [`(zero? ,expr1)
            (zero? (value-of expr1 env))]
          [`(,expr1 ,expr2)
            (apply-closure (value-of expr1 env) (value-of expr2 env) value-of)])))))

(define empty-env-ds (lambda () '(empty-env)))

(define extend-env-ds
  (lambda (id val env)
    `(extend-env ,id ,val ,env)))

(define apply-env-ds
  (lambda (env var)
    (match env
      ['(empty-env) (error "cannot find binding for ~s" var)]
      [`(extend-env ,id ,val ,env)
        (if (eqv? id var)
          val
          (apply-env-ds env var))]
      [e (printf "~s \n" e)]
      )))

(define empty-env-fn
  (lambda ()
    (lambda (var)
      (error "No binding for ~s" var))))

(define extend-env-fn
  (lambda (id val env)
    (lambda (var)
      (if (eqv? var id)
        val
        (apply-env-fn env var)))))

(define apply-env-fn
  (lambda (env var)
    (env var)))

(define closure-fn-ri
  (lambda (var body env extend-env)
    (lambda (value-of)
      (lambda (arg)
        (value-of body (extend-env var arg env))))))

(define apply-closure-fn-ri
  (lambda (closure arg value-of)
    ((closure value-of) arg)))

(define closure-ds-ri
  (lambda (var body env extend-env)
    (list var body env extend-env)))

(define apply-closure-ds-ri
  (lambda (closure arg value-of)
    (match closure
      [(list var body env extend-env)
       (value-of body (extend-env var arg env))])))
