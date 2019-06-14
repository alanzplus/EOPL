#lang eopl

(require "./implicit-refs-spec.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide proc-val)
(provide expval->num)
(provide expval->bool)
(provide expval->proc)
(provide procedure)
(provide apply-procedure)
(provide newref)
(provide setref!)

; -----------------------------------------------------------------------------
; Expression Value Representation
; -----------------------------------------------------------------------------
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?)))

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

; -----------------------------------------------------------------------------
; Procedure Representation
; -----------------------------------------------------------------------------
(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body expression?)
    (env environment?)))

; Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body env)
        (value-of body (extend-env var (newref val) env))))))

; -----------------------------------------------------------------------------
; Environment
; -----------------------------------------------------------------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val reference?)
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
            (newref (proc-val (procedure p-var p-body env)))
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
; Store
; -----------------------------------------------------------------------------

(define the-store 'uninitialized)

; () -> EmptyList
(define empty-store
  (lambda () '()))

; () -> Store
(define get-store
  (lambda () the-store))

(define reference?
  (lambda (v)
    (integer? v)))

; ExpVal -> Referenc (Integer)
(define newref
  (lambda (val)
    (let
      ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

; Reference -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

; Reference x ExpVal -> Undefined
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec
        ((setref-inner
            (lambda (store1 ref1)
              (cond
                ((null? store1)
                 (eopl:error "Cannot set store using reference ~s" ref1))
                ((zero? ref1)
                 (cons val (cdr store1)))
                (else
                  (cons
                    (car store1)
                    (setref-inner (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref)))))

; Initialization
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

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
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

; Expression x Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (assign-exp (var exp1)
        (let ((val (value-of exp1 env)))
             (begin
                (setref! (apply-env env var) val)
                val)))
      (letrec-exp (p-name p-var p-body letrec-body)
        (value-of letrec-body (extend-env-rec p-name p-var p-body env)))
      (call-exp (exp1 exp2)
        (let ((proc (expval->proc (value-of exp1 env)))
              (arg (value-of exp2 env)))
             (apply-procedure proc arg)))
      (proc-exp (var body) (proc-val (procedure var body env)))
      (diff-exp (exp1 exp2)
        (let ((arg1 (value-of exp1 env))
              (arg2 (value-of exp2 env)))
            (num-val
              (-
                (expval->num arg1)
                (expval->num arg2)))))
      (const-exp (num) (num-val num))
      (var-exp (var)
        (deref (apply-env env var)))
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env)))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
            (value-of exp2 env)
            (value-of exp3 env)))
      (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
                (value-of body (extend-env var (newref val1) env)))))))