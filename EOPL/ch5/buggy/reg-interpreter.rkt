#lang eopl

(require "./reg-spec.rkt")

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
; List Utilities
; -----------------------------------------------------------------------------

; Return the first idx which satifies the predicate
(define find-idx
  (lambda (predicate lst)
    (letrec ((helper
                (lambda (idx alst)
                    (if (null? alst)
                        -1
                        (if (predicate (car alst))
                            idx
                            (helper (+ idx 1) (cdr alst)))))))
            (helper 0 lst))))
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
    (vars (list-of identifier?))
    (body expression?)
    (env environment?)))

; Proc x ExpVal x Cont -> ExpVal
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (vars body saved-env)
        (eopl:pretty-print "~~~ apply-procedure/k")
        (eopl:pretty-print env)
        (eopl:pretty-print saved-env)
        (set! env (extend-env-list vars (newref-list val) saved-env))
        (set! exp body)
        (value-of/k)))))

; -----------------------------------------------------------------------------
; Store
; -----------------------------------------------------------------------------
(define the-store 'uninitialized)

; () -> Store
(define empty-store
  (lambda () '()))

; () -> Store
(define get-store
  (lambda () the-store))

; () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

; SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))

; ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

; ExpVals -> Refs
(define newref-list
  (lambda (vals)
    (map newref vals)))

; Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

; Ref x ExpVal -> Unspecified
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec
        ((setref-inner
          (lambda (store1 ref1)
            (cond
              ((null? store1)
                (eopl:error "cannot set ~s in reference ~s" val ref))
              ((zero? ref1)
               (cons val (cdr store1)))
              (else
                (cons
                  (car store1)
                  (setref-inner
                    (cdr store1)
                    (- ref1 1))))))))
        (setref-inner the-store ref)))))

; -----------------------------------------------------------------------------
; Environment
; -----------------------------------------------------------------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val reference?)
    (env environment?))
  (extend-env-list
    (vars (list-of identifier?))
    (vals (list-of reference?))
    (env environment?))
  (extend-env-rec
    (p-name identifier?)
    (p-vars (list-of identifier?))
    (body expression?)
    (env environment?)))

; Environment x Identifier -> ExpVal
(define apply-env
  (lambda (env1 search-var)
    (cases environment env1
      (empty-env ()
        (eopl:error "APPLY-ENV: there is no binding for" search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))
      (extend-env-list (saved-vars saved-vals saved-env)
        (let ((idx
                (find-idx
                   (lambda (ele)
                      (eqv? ele search-var))
                   saved-vars)))
              (if (eqv? idx -1)
                  (apply-env saved-env search-var)
                  (list-ref saved-vals idx))))
      (extend-env-rec (p-name p-vars p-body saved-env)
        (if (eqv? search-var p-name)
            (newref (proc-val (procedure p-vars p-body saved-env)))
            (apply-env saved-env search-var))))))

; Initilization
(define init-env
  (lambda ()
    (extend-env
      'i (newref (num-val 1))
      (extend-env
        'v (newref (num-val 5))
          (extend-env
            'x (newref (num-val 10))
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
  (mul1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (mul2-cont
    (val expval?)
    (cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    (val expval?)
    (cont continuation?))
  (rator-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (rand-cont
    (env environment?)
    (val expval?)
    (exps (list-of expression?))
    (vals (list-of expval?))
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
  (letmul-cont
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (body expression?)
    (body-eval-env environment?)
    (binding-eval-env environment?)
    (cont continuation?))
  (set-rhs-cont
    (env environment?)
    (var identifier?)
    (cont continuation?))
  (begin-exp-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
)

(define apply-cont
  (lambda ()
    (begin
      (eopl:pretty-print "----- apply-cont ------")
      (eopl:pretty-print cont)
      (eopl:pretty-print val)
      (apply-cont-inner))))

(define apply-cont-inner
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "---------------------\n")
          (eopl:printf "End of computation. Final answer: \n")
          (eopl:pretty-print val)
          (eopl:printf "---------------------\n\n")
          val))
      (zero-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (bool-val (zero? (expval->num val))))
        (apply-cont))
      (let-exp-cont (var body saved-env saved-cont)
        (set! cont saved-cont)
        (set! exp body)
        (set! env (extend-env var (newref val) saved-env))
        (value-of/k))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
            (begin
              (set! cont saved-cont)
              (set! env saved-env)
              (set! exp exp2)
              (value-of/k))
            (begin
              (set! cont saved-cont)
              (set! env saved-env)
              (set! exp exp3)
              (value-of/k))))
      (mul1-cont (exp2 saved-env saved-cont)
        (set! cont (mul2-cont val saved-cont))
        (set! env saved-env)
        (set! exp exp2)
        (value-of/k))
      (mul2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
             (begin
              (set! cont saved-cont)
              (set! val (num-val (* num1 num2)))
              (apply-cont))))
      (diff1-cont (exp2 saved-env saved-cont)
        (set! cont (diff2-cont val saved-cont))
        (set! env saved-env)
        (set! exp exp2)
        (value-of/k))
      (diff2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
             (begin
              (set! cont saved-cont)
              (set! val (num-val (- num1 num2)))
              (apply-cont))))
      (rator-cont (exps saved-env saved-cont)
        (set! cont (rand-cont saved-env val (cdr exps) '() saved-cont))
        (set! exp (car exps))
        (set! env saved-env)
        (value-of/k))
      (rand-cont (saved-env rator exps vals saved-cont)
        (if (null? exps)
            (begin
              (set! cont saved-cont)
              (set! proc1 (expval->proc rator))
              (set! val (append vals (list val)))
              (apply-procedure/k))
            (begin
              (set! cont (rand-cont saved-env rator (cdr exps) (append vals (list val)) saved-cont))
              (set! env saved-env)
              (set! exp (car exps))
              (value-of/k))))
      (let2-cont1 (var1 var2 exp2 body saved-env saved-cont)
        (set! cont (let2-cont2 var1 val var2 body saved-env saved-cont))
        (set! exp exp2)
        (set! env saved-env)
        (value-of/k))
      (let2-cont2 (var1 val1 var2 body saved-env saved-cont)
        (set! cont saved-cont)
        (set! env (extend-env var2 (newref val) (extend-env var1 (newref val1) saved-env)))
        (set! exp body))
      (let3-cont1 (var1 var2 exp2 var3 exp3 body saved-env saved-cont)
        (set! cont (let3-cont2 var1 val var2 var3 exp3 body saved-env saved-cont))
        (set! env saved-env)
        (set! exp exp2)
        (value-of/k))
      (let3-cont2 (var1 val1 var2 var3 exp3 body saved-env saved-cont)
        (set! cont (let3-cont3 var1 val1 var2 val var3 body saved-env saved-cont))
        (set! env saved-env)
        (set! exp exp3)
        (value-of/k))
      (let3-cont3 (var1 val1 var2 val2 var3 body saved-env saved-cont)
        (set! cont saved-cont)
        (set! env (extend-env var3 (newref val) (extend-env var2 (newref val2) (extend-env var1 (newref val1) saved-env))))
        (set! exp body)
        (value-of/k))
      (cons-cont1 (exp2 saved-env saved-cont)
        (set! cont (cons-cont2 val saved-cont))
        (set! env saved-env)
        (set! exp exp2)
        (value-of/k))
      (cons-cont2 (val1 saved-cont)
        (set! cont saved-cont)
        (set! val (list-val (list val1 val)))
        (apply-cont))
      (car-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (car (expval->list val)))
        (apply-cont))
      (cdr-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (cadr (expval->list val)))
        (apply-cont))
      (null?-cont (saved-cont)
        (cases expval val
          (list-val (alist)
            (set! cont saved-cont)
            (set! val (bool-val (null? alist)))
            (apply-cont))
          (else (eopl:error "expected list-val"))))
      (list-rest-cont (exps saved-env saved-cont)
        (set! cont (list-first-cont val saved-cont))
        (set! exp exps)
        (set! env saved-env)
        (value-of/k))
      (list-first-cont (val1 saved-cont)
        (set! cont saved-cont)
        (set! val (list-val (cons val1 (expval->list val))))
        (apply-cont))
      (letmul-cont (vars exps body body-eval-env binding-eval-env saved-cont)
        (if (null? exps)
            (begin
              (set! cont saved-cont)
              (set! env (extend-env (car vars) (newref val) body-eval-env))
              (set! exp body)
              (value-of/k))
            (begin
              (set!
                cont
                (letmul-cont
                  (cdr vars)
                  (cdr exps)
                  body
                  (extend-env (car vars) (newref val) body-eval-env)
                  binding-eval-env
                  saved-cont))
              (set! env binding-eval-env)
              (set! exp (car exps)))))
      (set-rhs-cont (saved-env var saved-cont)
        (set! cont saved-cont)
        (set! val (setref! (apply-env saved-env var) val))
        (apply-cont))
      (begin-exp-cont (other-exps saved-env saved-cont)
        (if (null? other-exps)
            (begin
              (set! cont saved-cont)
              (apply-cont))
            (begin
              (set! cont (begin-exp-cont (cdr other-exps) saved-env saved-cont))
              (set! exp (car other-exps))
              (set! env saved-env)
              (value-of/k))))
      (else (eopl:error "unkonw type of continuation. ~s" cont))
)))

; -----------------------------------------------------------------------------
; Global Register
; -----------------------------------------------------------------------------
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)

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
        (initialize-store!)
        (set! cont (end-cont))
        (set! exp exp1)
        (set! env (init-env))
        (value-of/k)))))

; Expression X Environemnt x Continutation -> FinalAnswer (ExpVal)
(define value-of/k
  (lambda ()
    (begin
      (eopl:pretty-print "==========================")
      (eopl:pretty-print exp)
      (eopl:pretty-print env)
      (eopl:pretty-print cont)
      (value-of/k-inner))))

(define value-of/k-inner
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! val (num-val num))
        (apply-cont))
      (var-exp (var)
        (set! val (deref (apply-env env var)))
        (apply-cont))
      (proc-exp (vars body)
        (set! val (proc-val (procedure vars body env))) 
        (apply-cont))
      (letrec-exp (p-name p-vars p-body letrec-body)
        (set! exp letrec-body)
        (set! env (extend-env-rec p-name p-vars p-body env))
        (value-of/k))
      (zero?-exp (exp1)
        (set! cont (zero-cont cont))
        (set! exp exp1)
        (value-of/k))
      (let-exp (var exp1 body)
        (set! cont (let-exp-cont var body env cont))
        (set! exp exp1)
        (value-of/k))
      (if-exp (exp1 exp2 exp3)
        (set! cont (if-test-cont exp2 exp3 env cont))
        (set! exp exp1)
        (value-of/k))
      (mul-exp (exp1 exp2)
        (set! cont (mul1-cont exp2 env cont))
        (set! exp exp1)
        (value-of/k))
      (diff-exp (exp1 exp2)
        (set! cont (diff1-cont exp2 env cont))
        (set! exp exp1)
        (value-of/k))
      (call-exp (exp1 exps)
        (set! cont (rator-cont exps env cont))
        (set! exp exp1)
        (value-of/k))
      (let2-exp (var1 exp1 var2 exp2 body)
        (set! cont (let2-cont1 var1 var2 exp2 body env cont))
        (set! exp exp1)
        (value-of/k))
      (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
        (set! cont (let3-cont1 var1 var2 exp2 var3 exp3 body env cont))
        (set! exp exp1)
        (value-of/k))
      (cons-exp (exp1 exp2)
        (set! cont (cons-cont1 exp2 env cont))
        (set! exp exp1)
        (value-of/k))
      (car-exp (exp1)
        (set! cont (car-cont cont))
        (set! exp exp1)
        (value-of/k))
      (cdr-exp (exp1)
        (set! cont (cdr-cont cont))
        (set! exp exp1)
        (value-of/k))
      (null?-exp (exp1)
        (set! cont (null?-cont cont))
        (set! exp exp1)
        (value-of/k))
      (emptylist-exp ()
        (set! val (list-val '()))
        (apply-cont))
      (list-exp (exps)
        (if (null? exps)
            (begin
              (set! val (list-val '()))
              (apply-cont))
            (begin
              (set! cont (list-rest-cont (list-exp (cdr exps)) env cont))
              (set! exp exps)
              (value-of/k))))
      (letmul-exp (vars exps body)
        (set! cont (letmul-cont vars (cdr exps) body env env cont))
        (set! exp (car exps))
        (value-of/k))
      (assign-exp (var exp1)
        (set! cont (set-rhs-cont env var cont))
        (set! exp exp1)
        (value-of/k))
      (begin-exp (exp-first other-exps)
        (set! cont (begin-exp-cont other-exps env cont))
        (set! exp exp-first)
        (value-of/k))
      (else (eopl:error "cannot handle expression: ~s" exp))
)))

(run "letrec fun(k) =  if zero?(k) then 1 else (fun -(k,1)) in (fun 1)")