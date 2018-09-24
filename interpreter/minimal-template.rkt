#lang eopl

(require "../parser/minimal-spec.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide list-val)
(provide proc-val)
(provide procedure)
(provide apply-procedure)
(provide empty-list-val)
(provide expval->num)
(provide expval->bool)
(provide expval->proc)

; Environment

(define empty-env
  (lambda ()
    (lambda (search-var)
      (eopl:error "No binding found for ~s" search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (list-val
    (listval list?))
  (empty-list-val)
  (proc-val
    (proc proc?))
)

(define proc?
  (lambda (val)
    (procedure? val)))

(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env)))))

(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error "expected num-val")))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error "expected bool-val")))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (eopl:error "expected proc-val")))))

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

(define run
  (lambda (text)
    (value-of-program (let-scan-parse text))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

; interpretation
(define value-of
  (lambda (exp env)
    (cases expression exp
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
