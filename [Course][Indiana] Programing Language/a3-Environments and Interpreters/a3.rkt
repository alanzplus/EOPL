#lang racket

(provide parse)
(provide num-expr)
(provide var-expr)
(provide boolean-expr)
(provide lambda-expr)
(provide binding-expr)
(provide let-expr)
(provide if-expr)
(provide sub1-expr)
(provide mul-expr)
(provide zero?-expr)
(provide call-expr)
(provide value-of)
(provide value-of-fn)
(provide empty-env-fn)
(provide extend-env-fn)
(provide apply-env-fn)
(provide value-of-ds)
(provide empty-env-ds)
(provide extend-env-ds)
(provide apply-env-ds)
(provide fo-eulav)
(provide empty-env)
(provide value-of-lex)
(provide empty-env-lex)
(provide apply-env-lex)
(provide extend-env-lex)

(struct expression () #:transparent)

(struct num-expr expression (n)
  #:transparent
  #:guard (lambda (n name)
            (unless (number? n)
              (error name " expected number" ))
            n))

(struct var-expr expression (id)
  #:transparent
  #:guard (lambda (id name)
            (unless (symbol? id)
              (error name " expected symbol"))
            id))

(struct boolean-expr expression (b)
  #:transparent
  #:guard (lambda (b name)
            (unless (boolean? b)
              (error name " expected boolean"))
            b))

(struct lambda-expr expression (id body)
  #:transparent
  #:guard (lambda (id body name)
            (begin
              (unless (symbol? id)
                (error name " expected symbol"))
              (unless (expression? body)
                (error name " expected expression"))
              (values id body))))

(struct binding-expr (id init-expr)
  #:transparent
  #:guard (lambda (id init-expr name)
            (begin
              (unless (symbol? id)
                (error name " expected symbol"))
              (unless (expression? init-expr)
                (error name " expected expression"))
              (values id init-expr))))

(struct let-expr expression (bindings body)
  #:transparent
  #:guard (lambda (bindings body name)
            (begin
              (unless (andmap binding-expr? bindings)
                (error name " expected a list of bindings"))
              (unless (expression? body)
                (error name " expected expression"))
              (values bindings body))))

(struct sub1-expr expression (expr)
  #:transparent
  #:guard (lambda (expr name)
            (unless (expression? expr)
              (error name " expected expression"))
            expr))

(struct mul-expr expression (expr1 expr2)
  #:transparent
  #:guard (lambda (expr1 expr2 name)
            (begin
              (unless (expression? expr1)
                (error name " expected expression"))
              (unless (expression? expr2)
                (error name " expected expression"))
              (values expr1 expr2))))

(struct if-expr expression (pred-expr then-expr else-expr)
  #:transparent
  #:guard (lambda (pred-expr then-expr else-expr name)
            (begin
              (unless (expression? pred-expr)
                (error name " expected expression"))
              (unless (expression? then-expr)
                (error name " expected expression"))
              (unless (expression? else-expr)
                (error name " expected expression"))
              (values pred-expr then-expr else-expr))))

(struct zero?-expr expression (exp1)
  #:transparent
  #:guard (lambda (exp1 name)
            (unless (expression? exp1)
              (error name " expected expression"))
            exp1))

(struct call-expr expression (exp1 exp2)
  #:transparent
  #:guard (lambda (exp1 exp2 name)
            (begin
              (unless (expression? exp1)
                (error name " expected expression"))
              (unless (expression? exp2)
                (error name " expected expression"))
              (values exp1 exp2))))

(define (parse expr)
  (match expr
    [num #:when (number? num) (num-expr num)]
    [var #:when (symbol? var) (var-expr var)]
    [b #:when (boolean? b) (boolean-expr b)]
    [`(lambda (,id) ,body) (lambda-expr id (parse body))]
    [`(let ,bindings ,body)
      (let-expr
        (map
          (lambda (b)
            (binding-expr (car b) (parse (cadr b))))
          bindings)
        (parse body))]
    [`(sub1 ,expr1) (sub1-expr (parse expr1))]
    [`(* ,expr1 ,expr2) (mul-expr (parse expr1) (parse expr2))]
    [`(if ,pred ,then ,else) (if-expr (parse pred) (parse then) (parse else))]
    [`(zero? ,expr1) (zero?-expr (parse expr1))]
    [`(,expr1 ,expr2) (call-expr (parse expr1) (parse expr2))]))

(define value-of
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (unbox (env var))]
      [b #:when (boolean? b) b]
      [`(lambda (,id) ,body)
        (let ([arg-box (box 'null)])
          (lambda (arg)
            (set-box! arg-box arg)
            (value-of body
                      (lambda (var)
                        (if (eqv? var id)
                          arg-box
                          (env var))))))]
      [`(let ,bindings ,body)
        (value-of
          body
          (foldr
            (lambda (binding aggregate-env)
              (let ([id (car binding)]
                    [val (box (value-of (cadr binding) env))])
                (lambda (var)
                  (if (eqv? var id)
                    val
                    (aggregate-env var)))))
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
      [`(set! ,var ,expr1)
        (set-box! (env var) (value-of expr1 env))]
      [`(begin2 ,expr1 ,expr2)
        (begin
          (value-of expr1 env)
          (value-of expr2 env))]
      [`(,expr1 ,expr2)
        ((value-of expr1 env) (value-of expr2 env))])))

(define value-of-fn
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (apply-env-fn env var)]
      [b #:when (boolean? b) b]
      [`(lambda (,id) ,body)
        (lambda (arg)
          (value-of-fn body
                       (extend-env-fn id arg env)))]
      [`(let ,bindings ,body)
        (value-of-fn
          body
          (foldr
            (lambda (binding aggregate-env)
              (let ([id (car binding)]
                    [val (value-of-fn (cadr binding) env)])
                (extend-env-fn id val aggregate-env)))
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
        ((value-of-fn expr1 env) (value-of-fn expr2 env))])))

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

(define value-of-ds
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (apply-env-ds env var)]
      [b #:when (boolean? b) b]
      [`(lambda (,id) ,body)
        (lambda (arg)
          (value-of-ds
            body
            (extend-env-ds id arg env)))]
      [`(let ,bindings ,body)
        (value-of-ds
          body
          (foldr
            (lambda (binding aggregate-env)
              (let ([id (car binding)]
                    [val (value-of-ds (cadr binding) env)])
                (extend-env-ds id val aggregate-env)))
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
        ((value-of-ds expr1 env) (value-of-ds expr2 env))])))

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
          (apply-env-ds env var))])))

(define fo-eulav
  (lambda (expr env)
    (match expr
      [num #:when (number? num) num]
      [var #:when (symbol? var) (env var)]
      [b #:when (boolean? b) b]
      [`(,body (,id) adbmal)
        (lambda (arg)
          (fo-eulav
            body
            (lambda (var)
              (if (eqv? var id)
                arg
                (env var)))))]
      [`(,else-expr ,then-expr ,pred-expr fi)
        (if (fo-eulav pred-expr env)
          (fo-eulav then-expr env)
          (fo-eulav else-expr env))]
      [`(,expr1 ?orez)
        (zero? (fo-eulav expr1 env))]
      [`(,expr1 1bus)
        (- (fo-eulav expr1 env) 1)]
      [`(,expr1 ,expr2 *)
        (* (fo-eulav expr1 env) (fo-eulav expr2 env))]
      [`(,expr1 ,expr2)
        ((fo-eulav expr2 env) (fo-eulav expr1 env))]
      [e (printf "~s\n" e)])))

(define empty-env
  (lambda ()
    (lambda (var)
      (error "cannot find binding of " var))))

(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define extend-env-lex
  (lambda (val env)
    (cons val env)))

(define apply-env-lex
  (lambda (env num)
    (list-ref env num)))
