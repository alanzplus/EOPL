#lang racket

(provide val-of-cbr)
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
