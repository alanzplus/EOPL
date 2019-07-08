#lang racket

(provide last-non-zero)
(provide lex)
(provide value-of)
(provide empty-env)
(provide empty-k)
(provide value-of-cps)

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

(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

(define empty-k
  (lambda ()
    (lambda (v)
      (displayln "should print only once")
      v)))

(define value-of-cps
  (lambda (expr env cont)
    (match expr
           [`(const ,val) (apply-k cont val)]
           [`(mult ,x1 ,x2)
             (value-of-cps x1 env (mult-cont x1 x2 env cont))]
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

; Procedure Presentation for Continuation
(define mult-cont
  (lambda (x1 x2 env saved-cont)
    (lambda (v1)
      (value-of-cps x2 env (lambda (v2)
                             (apply-k saved-cont (* v1 v2)))))))

(define sub1-cont
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont (- v1 1)))))

(define zero-cont
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont (zero? v1)))))

(define if-cont
  (lambda (conseq alt env saved-cont)
    (lambda (v1)
      (if v1
          (value-of-cps conseq env (lambda (v2) (apply-k saved-cont v2)))
          (value-of-cps alt env (lambda (v2) (apply-k saved-cont v2)))))))

(define letcc-cont
  (lambda (saved-cont)
    (lambda (v1)
      (apply-k saved-cont v1))))

(define throw-cont
  (lambda (env saved-cont v-exp)
    (lambda (v1)
      (value-of-cps v-exp env (lambda (v2) (v1 v2))))))

(define let-cont
  (lambda (env cont body)
    (lambda (v1)
      (value-of-cps body
                    (extend-env v1 env)
                    (lambda (v2) (apply-k cont v2))))))

(define app-cont
  (lambda (env saved-cont rand)
    (lambda (v1)
      (value-of-cps rand env (lambda (v2) (apply-closure v1 v2 saved-cont))))))

(define make-closure
  (lambda (body env)
    (lambda (a k)
      (value-of-cps body
                    (extend-env a env)
                    (lambda (v1)
                      (apply-k k v1))))))

(define apply-env
  (lambda (env address)
    (env address)))

(define extend-env
  (lambda (val env)
    (lambda (y)
      (if (zero? y) val
          (env (sub1 y))))))

(define apply-closure
  (lambda (rator rand cont)
    (rator rand cont)))

(define apply-k
  (lambda (cont v)
    (cont v)))
