#lang racket

(provide val-of-cbr)
(provide val-of-cbv)
(provide val-of-cbname)
(provide empty-env)

(define val-of-cbr
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (unbox (apply-env env var))]
      [b #:when (boolean? b) b]
      [`(* ,expr1 ,expr2)
        (* (val-of-cbr expr1 env) (val-of-cbr expr2 env))]
      [`(sub1 ,expr1)
        (sub1 (val-of-cbr expr1 env))]
      [`(zero? ,expr1)
        (zero? (val-of-cbr expr1 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (val-of-cbr pred-expr env)
          (val-of-cbr then-expr env)
          (val-of-cbr else-expr env))]
      [`(random ,n)
        (random (val-of-cbr n env))]
      [`(begin2 ,expr1 ,expr2)
        (begin
          (val-of-cbr expr1 env)
          (val-of-cbr expr2 env))]
      [`(set! ,var ,expr1)
        (set-box! (apply-env env var) (val-of-cbr expr1 env))]
      [`(lambda (,var) ,body)
        (make-closure-cbr var body env)]
      [`(let ,bindings ,body)
        (val-of-cbr
          body
          (foldr
            (lambda (binding new-env)
              (let
                ([var (car binding)]
                 [val (val-of-cbr (cadr binding) env)])
                (extend-env var (box val) new-env)))
            env
            bindings))]
      [`(,expr1 ,var) #:when (symbol? var)
        (apply-closure (val-of-cbr expr1 env) (apply-env env var))]
      [`(,expr1 ,expr2)
        (apply-closure (val-of-cbr expr1 env) (box (val-of-cbr expr2 env)))])))

(define val-of-cbv
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (unbox (apply-env env var))]
      [b #:when (boolean? b) b]
      [`(* ,expr1 ,expr2)
        (* (val-of-cbv expr1 env) (val-of-cbv expr2 env))]
      [`(sub1 ,expr1)
        (sub1 (val-of-cbv expr1 env))]
      [`(zero? ,expr1)
        (zero? (val-of-cbv expr1 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (val-of-cbv pred-expr env)
          (val-of-cbv then-expr env)
          (val-of-cbv else-expr env))]
      [`(random ,n)
        (random (val-of-cbv n env))]
      [`(begin2 ,expr1 ,expr2)
        (begin
          (val-of-cbv expr1 env)
          (val-of-cbv expr2 env))]
      [`(set! ,var ,expr1)
        (set-box! (apply-env env var) (val-of-cbv expr1 env))]
      [`(lambda (,var) ,body)
        (make-closure-cbv var body env)]
      [`(let ,bindings ,body)
        (val-of-cbv
          body
          (foldr
            (lambda (binding new-env)
              (let
                ([var (car binding)]
                 [val (val-of-cbv (cadr binding) env)])
                (extend-env var (box val) new-env)))
            env
            bindings))]
      [`(,expr1 ,var) #:when (symbol? var)
                      (apply-closure (val-of-cbv expr1 env) (apply-env env var))]
      [`(,expr1 ,expr2)
        (apply-closure (val-of-cbv expr1 env) (val-of-cbv expr2 env))])))

(define val-of-cbname
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) ((apply-env env var))]
      [b #:when (boolean? b) b]
      [`(* ,expr1 ,expr2)
        (* (val-of-cbname expr1 env) (val-of-cbname expr2 env))]
      [`(sub1 ,expr1)
        (sub1 (val-of-cbname expr1 env))]
      [`(zero? ,expr1)
        (zero? (val-of-cbname expr1 env))]
      [`(if ,pred-expr ,then-expr ,else-expr)
        (if (val-of-cbname pred-expr env)
          (val-of-cbname then-expr env)
          (val-of-cbname else-expr env))]
      [`(random ,n)
        (random (val-of-cbname n env))]
      [`(lambda (,var) ,body)
        (make-closure-cbname var body env)]
      [`(let ,bindings ,body)
        (val-of-cbname
          body
          (foldr
            (lambda (binding new-env)
              (let
                ([var (car binding)]
                 [val (lambda () (val-of-cbname (cadr binding) env))])
                (extend-env var val new-env)))
            env
            bindings))]
      [`(,expr1 ,var) #:when (symbol? var)
                      (apply-closure (val-of-cbname expr1 env) ((apply-env env var)))]
      [`(,expr1 ,expr2)
        (apply-closure (val-of-cbname expr1 env) (lambda () (val-of-cbname expr2 env)))])))

(define make-closure-cbname
  (lambda (var body env)
    (lambda (arg)
      (val-of-cbname
        body
        (extend-env var arg env)))))

(define make-closure-cbv
  (lambda (var body env)
    (lambda (arg)
      (let ([boxed-arg (box arg)])
        (val-of-cbv
          body
          (extend-env var boxed-arg env))))))

(define make-closure-cbr
  (lambda (var body env)
    (lambda (arg)
      (val-of-cbr
        body
        (extend-env var arg env)))))

(define apply-closure
  (lambda (closure arg)
    (closure arg)))

(define empty-env
  (lambda ()
    (lambda (var)
      (error "cannot find binding of ~s" var))))

(define extend-env
  (lambda (saved-var val env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
        val
        (apply-env env search-var)))))

(define apply-env
  (lambda (env var)
    (env var)))
