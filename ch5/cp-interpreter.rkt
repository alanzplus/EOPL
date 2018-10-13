#lang eopl

(require "./cp-spec.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide proc-val)
(provide list-val)
(provide expval->num)
(provide expval->bool)
(provide expval->proc)
(provide expval->list)
(provide procedure)
(provide apply-procedure/k)

; -----------------------------------------------------------------------------
; Expression Value Representation
; -----------------------------------------------------------------------------
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?))
  (list-val
    (alist list?))
)

; ExpVal -> num
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error "expected num-val")))))

; ExpVal -> bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error "expected bool-val")))))

; ExpVal -> Procedure
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (eopl:error "expected proc-val")))))

; ExpVal -> list
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (alist) alist)
      (else (eopl:error "expected list-val")))))

; -----------------------------------------------------------------------------
; Procedure Representation
; -----------------------------------------------------------------------------
(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body expression?)
    (env environment?)))

; Proc x ExpVal x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body env)
        (value-of/k body (extend-env var val env) cont)))))

; -----------------------------------------------------------------------------
; Environment
; -----------------------------------------------------------------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-name identifier?)
    (p-var identifier?)
    (body expression?)
    (env environment?)))

; Environment x Identifier -> ExpVal
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (eopl:error "there is no binding for ~s" search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))
      (extend-env-rec (p-name p-var p-body saved-env)
        (if (eqv? search-var p-name)
            (proc-val (procedure p-var p-body env))
            (apply-env saved-env search-var))))))

; Initilization
(define init-env
  (lambda ()
    (extend-env
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
          (extend-env
            'x (num-val 10)
            (empty-env))))))

; -----------------------------------------------------------------------------
; Continuation
; -----------------------------------------------------------------------------
(define-datatype continuation continuation?
  (end-cont)
  (zero-cont
    (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    (val expval?)
    (cont continuation?))
  (rator-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (val expval?)
    (cont continuation?))
  (let2-cont1
    (var1 identifier?)
    (var2 identifier?)
    (exp2 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let2-cont2
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont1
    (var1 identifier?)
    (var2 identifier?)
    (exp2 expression?)
    (var3 identifier?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont2
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (var3 identifier?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont3
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (val2 expval?)
    (var3 identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (cons-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (cons-cont2
    (val expval?)
    (cont continuation?))
  (car-cont
    (cont continuation?))
  (cdr-cont
    (cont continuation?))
  (null?-cont
    (cont continuation?))
  (list-first-cont
    (val expval?)
    (cont continuation?))
  (list-rest-cont
    (exps expression?)
    (env environment?)
    (cont continuation?))
)

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "---------------------\n")
          (eopl:printf "End of computation. Final answer: \n")
          (eopl:pretty-print val)
          (eopl:printf "---------------------\n\n")
          val))
      (zero-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k
          body
          (extend-env var val saved-env)
          saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 env saved-cont)
        (value-of/k
          exp2
          env
          (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
            (apply-cont saved-cont (num-val (- num1 num2)))))
      (rator-cont (exp2 env saved-cont)
        (value-of/k
          exp2
          env
          (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
        (let ((p (expval->proc val1)))
          (apply-procedure/k p val saved-cont)))
      (let2-cont1 (var1 var2 exp2 body env saved-cont)
        (value-of/k exp2 env (let2-cont2 var1 val var2 body env saved-cont)))
      (let2-cont2 (var1 val1 var2 body env saved-cont)
        (value-of/k body (extend-env var2 val (extend-env var1 val1 env)) saved-cont))
      (let3-cont1 (var1 var2 exp2 var3 exp3 body env saved-cont)
        (value-of/k exp2 env (let3-cont2 var1 val var2 var3 exp3 body env saved-cont)))
      (let3-cont2 (var1 val1 var2 var3 exp3 body env saved-cont)
        (value-of/k exp3 env (let3-cont3 var1 val1 var2 val var3 body env saved-cont)))
      (let3-cont3 (var1 val1 var2 val2 var3 body env saved-cont)
        (value-of/k body (extend-env var3 val (extend-env var2 val2 (extend-env var1 val1 env))) saved-cont))
      (cons-cont1 (exp2 env saved-cont)
        (value-of/k exp2 env (cons-cont2 val saved-cont)))
      (cons-cont2 (val1 saved-cont)
        (apply-cont saved-cont (list-val (list val1 val))))
      (car-cont (saved-cont)
        (apply-cont saved-cont (car (expval->list val))))
      (cdr-cont (saved-cont)
        (apply-cont saved-cont (cadr (expval->list val))))
      (null?-cont (saved-cont)
        (cases expval val
          (list-val (alist) (apply-cont saved-cont (bool-val (null? alist))))
          (else (eopl:error "expected list-val"))))
      (list-rest-cont (exps env saved-cont)
          (value-of/k exps env (list-first-cont val saved-cont)))
      (list-first-cont (val1 saved-cont)
          (apply-cont saved-cont (list-val (cons val1 (expval->list val)))))
      (else (eopl:error "unkonw type of continuation. ~s" cont))
)))

; -----------------------------------------------------------------------------
; Interpreter
; -----------------------------------------------------------------------------

; String -> ExpVal
(define run
  (lambda (text)
    (value-of-program (scan-parse text))))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))))))

; Expression X Environemnt x Continutation -> FinalAnswer (ExpVal)
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont (proc-val (procedure var body env))))
      (letrec-exp (p-name p-var p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name p-var p-body env)
          cont))
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero-cont cont)))
      (let-exp (var exp1 body)
        (value-of/k
          exp1
          env
          (let-exp-cont var body env cont)))
      (if-exp (exp1 exp2 exp3)
        (value-of/k
          exp1
          env
          (if-test-cont exp2 exp3 env cont)))
      (diff-exp (exp1 exp2)
        (value-of/k
          exp1
          env
          (diff1-cont exp2 env cont)))
      (call-exp (exp1 exp2)
        (value-of/k
          exp1
          env
          (rator-cont exp2 env cont)))
      (let2-exp (var1 exp1 var2 exp2 body)
        (value-of/k
          exp1 env (let2-cont1 var1 var2 exp2 body env cont)))
      (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
        (value-of/k
          exp1 env (let3-cont1 var1 var2 exp2 var3 exp3 body env cont)))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env (cons-cont1 exp2 env cont)))
      (car-exp (exp1)
        (value-of/k exp1 env (car-cont cont)))
      (cdr-exp (exp1)
        (value-of/k exp1 env (cdr-cont cont)))
      (null?-exp (exp1)
        (value-of/k exp1 env (null?-cont cont)))
      (emptylist-exp ()
        (apply-cont cont (list-val '())))
      (list-exp (exps)
        (if (null? exps)
            (apply-cont cont (list-val '()))
            (value-of/k
              (car exps)
              env
              (list-rest-cont (list-exp (cdr exps)) env cont))))
      (else (eopl:error "cannot handle expression: ~s" exp))
)))