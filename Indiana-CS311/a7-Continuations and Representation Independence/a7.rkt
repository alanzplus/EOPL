#lang racket

(provide last-non-zero)
(provide lex)
(provide value-of)
(provide empty-env)
(provide empty-k)
(provide value-of-cps)
(provide take$)
;(provide inf-1s)
(provide car$)
(provide cdr$)
(provide trib$)

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

; value-of accepts lexed expression, that is the output of lex
(define value-of
  (lambda (expr env)
    (match expr
           [`(const ,expr) expr]
           [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
           [`(sub1 ,x) (sub1 (value-of x env))]
           [`(zero ,x) (zero? (value-of x env))]
           [`(if ,test ,conseq ,alt) (if (value-of test env)
                                         (value-of conseq env)
                                         (value-of alt env))]
           [`(letcc ,body) (let/cc k
                                   (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
           [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
           [`(let ,e ,body) (let ((a (value-of e env)))
                              (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
           [`(var ,y) (env y)]
           [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
           [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))

; Closure
(define apply-closure
  (lambda (rator rand cont)
    (rator rand cont)))

; Continuation
;   Procedure Presentation
(define empty-env-fun
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

(define extend-env-fun
  (lambda (val env)
    (lambda (y)
      (if (zero? y) val
          (env (sub1 y))))))

(define apply-env-fun
  (lambda (env address)
    (env address)))

; Continuation
;   Data Structure Presentation
(struct empty-env-ds ())

(struct extend-env-ds (val saved-env))

(define apply-env-ds
  (lambda (x address)
    (match x
           [(extend-env-ds val saved-env)
            (if (zero? address) val (apply-env saved-env (sub1 address)))]
           [(empty-env-ds ) (error 'value-of-cps "unbound identifier")])))

; Continuation
;   Procedure Presentation
(define empty-k-fun
  (lambda ()
    (lambda (v)
      (displayln "should print only once")
      v)))

(define mult-cont-fun
  (lambda (x2 env saved-cont)
    (lambda (v1)
      (value-of-cps x2 env (lambda (v2)
                             (apply-k saved-cont (* v1 v2)))))))

(define sub1-cont-fun
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont (- v1 1)))))

(define zero-cont-fun
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont (zero? v1)))))

(define if-cont-fun
  (lambda (conseq alt env saved-cont)
    (lambda (v1)
      (if v1
          (value-of-cps conseq env (lambda (v2) (apply-k saved-cont v2)))
          (value-of-cps alt env (lambda (v2) (apply-k saved-cont v2)))))))

(define letcc-cont-fun
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont v1))))

(define throw-cont-fun
  (lambda (env saved-cont v-exp)
    (lambda (v1)
      (value-of-cps v-exp env (lambda (v2) (apply-k-fun v1 v2))))))

(define let-cont-fun
  (lambda (env cont body)
    (lambda (v1)
      (value-of-cps body
                    (extend-env v1 env)
                    (lambda (v2) (apply-k cont v2))))))

(define app-cont-fun
  (lambda (env saved-cont rand)
    (lambda (v1)
      (value-of-cps rand env (lambda (v2) (apply-closure v1 v2 saved-cont))))))

(define apply-k-fun
  (lambda (cont v) (cont v)))

(define make-closure-fun
  (lambda (body env)
    (lambda (a k)
      (value-of-cps body
                    (extend-env a env)
                    (lambda (v1)
                      (apply-k-fun k v1))))))

; Continuation
;   Data Structure Presentation
(struct empty-k-ds ())
(struct mult-cont-ds (x2 saved-env saved-cont))
(struct mult-cont-inner-ds (v saved-cont))
(struct sub1-cont-ds (saved-cont))
(struct zero-cont-ds (saved-cont))
(struct if-cont-ds (conseq alt saved-env saved-cont))
(struct if-conseq-cont-ds (saved-cont))
(struct if-alt-cont-ds (saved-cont))
(struct letcc-cont-ds (saved-cont))
(struct throw-cont-ds (saved-env saved-cont v-exp))
(struct throw-inner-cont-ds (saved-cont v))
(struct let-cont-ds (saved-env saved-cont body))
(struct let-inner-cont-ds (saved-cont))
(struct app-cont-ds (saved-env saved-cont rand))
(struct app-inner-cont-ds (v saved-cont))
(struct closure-cont (saved-cont))

(define apply-k-ds
  (lambda (cont v)
    (match cont
           [(empty-k-ds) v]
           [(mult-cont-ds x2 saved-env saved-cont)
            (value-of-cps x2 saved-env (mult-cont-inner-ds v saved-cont))]
           [(mult-cont-inner-ds v1 saved-cont)
            (apply-k-ds saved-cont (* v1 v))]
           [(sub1-cont-ds saved-cont)
            (apply-k-ds saved-cont (- v 1))]
           [(zero-cont-ds saved-cont)
            (apply-k-ds saved-cont (zero? v))]
           [(if-cont-ds conseq alt saved-env saved-cont)
            (if v
                (value-of-cps conseq saved-env (if-conseq-cont-ds saved-cont))
                (value-of-cps alt saved-env (if-alt-cont-ds saved-cont)))]
           [(if-conseq-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(if-alt-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(letcc-cont-ds saved-cont)
            (apply-k-ds saved-cont v)]
           [(throw-cont-ds saved-env saved-cont v-exp)
            (value-of-cps v-exp saved-env (throw-inner-cont-ds saved-cont v))]
           [(throw-inner-cont-ds saved-cont v1) (apply-k-ds v1 v)]
           [(let-cont-ds saved-env saved-cont body)
            (value-of-cps body
                          (extend-env v saved-env)
                          (let-inner-cont-ds saved-cont))]
           [(let-inner-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(app-cont-ds saved-env saved-cont rand)
            (value-of-cps rand saved-env (app-inner-cont-ds v saved-cont))]
           [(app-inner-cont-ds v1 saved-cont)
            (apply-closure v1 v saved-cont)]
           [(closure-cont saved-cont)
            (apply-k-ds saved-cont v)]
           )))

(define make-closure-ds
  (lambda (body env)
    (lambda (a k)
      (value-of-cps body (extend-env a env) (closure-cont k)))))

; Bind to different implementations for environment and continuation
(define mult-cont mult-cont-ds)
(define sub1-cont sub1-cont-ds)
(define zero-cont zero-cont-ds)
(define if-cont if-cont-ds)
(define letcc-cont letcc-cont-ds)
(define throw-cont throw-cont-ds)
(define let-cont let-cont-ds)
(define app-cont app-cont-ds)
(define apply-k apply-k-ds)
(define make-closure make-closure-ds)
(define empty-k empty-k-ds)

(define empty-env empty-env-ds)
(define extend-env extend-env-ds)
(define apply-env apply-env-ds)

(define value-of-cps
  (lambda (expr env cont)
    (match expr
           [`(const ,val) (apply-k cont val)]
           [`(mult ,x1 ,x2)
             (value-of-cps x1 env (mult-cont x2 env cont))]
           [`(sub1 ,x) (value-of-cps x env (sub1-cont cont))]
           [`(zero ,x) (value-of-cps x env (zero-cont cont))]
           [`(if ,test ,conseq ,alt) (value-of-cps test env (if-cont conseq alt env cont))]
           [`(letcc ,body) (value-of-cps body (extend-env cont env) (letcc-cont cont))]
           [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (throw-cont env cont v-exp))]
           [`(let ,e ,body) (value-of-cps e env (let-cont env cont body))]
           [`(lambda ,body)
             (apply-k cont (make-closure body env))]
           [`(app ,rator ,rand) (value-of-cps rator env (app-cont env cont rand))]
           [`(var ,address) (apply-k cont (apply-env env address))])))


(define thaw
  (lambda (thunk) (thunk)))

(define make-promise
  (lambda (thunk)
    (let ([res 'uninit])
      (lambda ()
        (if (eq? res 'uninit)
            (begin
              (set! res (thaw thunk))
              res)
            (begin
              res))))))

(define force thaw)

(define-syntax freeze
  (syntax-rules ()
    [(_ exprs ...) (lambda () exprs ...)]))

(define-syntax delay
  (syntax-rules ()
    [(_ exprs ...) (make-promise (freeze exprs ...))]))

(define-syntax cons$
  (syntax-rules ()
    [(_ expr stream) (cons expr (delay stream))]))

(define car$ car)

(define cdr$ (compose force cdr))

(define take$
  (lambda (n $)
    (cond
      [(zero? n) '()]
      [else (cons (car$ $) (take$ (sub1 n) (cdr$ $)))])))

(define trib$
  (letrec
    ([stream-builder
       (lambda ($)
         (let ([e1 (car$ $)]
               [e2 (car$ (cdr$ $))]
               [e3 (car$ (cdr$ (cdr$ $)))])
           (cons$ (+ e1 e2 e3) (stream-builder (cdr$ $)))))])
    (cons$
      0
      (cons$
        1
        (cons$
          1
          (stream-builder trib$))))))
