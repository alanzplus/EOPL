#lang eopl

(require "./A-spec.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide proc-val)
(provide expval->num)
(provide expval->bool)
(provide expval->proc)
(provide procedure)
(provide apply-procedure)

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
        (value-of body (extend-env var val env))))))

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
        (value-of exp1 (init-env))))))

; Expression x Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (begin-exp (exp1 other-exps)
        (let ((exp-list (cons exp1 other-exps)))
          (let ((val-list (map (lambda (ele) (value-of ele env)) exp-list)))
            (list-ref val-list (- (length val-list) 1)))))
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
      (var-exp (var) (apply-env env var))
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env)))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
            (value-of exp2 env)
            (value-of exp3 env)))
      (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
                (value-of body (extend-env var val1 env)))))))