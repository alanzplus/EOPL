#lang eopl

(require "../parser/let-spec-minimal.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide proc-val)

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)))

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

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

(define empty-nameless-env
  (lambda () '()))

(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body
          (extend-nameless-env val saved-nameless-env))))))

(define empty-senv
  (lambda () '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv)
       (eopl:error "no binding for ~s" var))
      ((eqv? var (car senv)) 0) (else
        (+ 1 (apply-senv (cdr senv) var))))))

(define init-senv
  (lambda ()
    (extend-senv 'i
      (extend-senv 'v
        (extend-senv 'x
          (empty-senv))))))

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (translation-of exp1 (init-senv))))))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)))
      (zero?-exp (exp1)
        (zero?-exp
          (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)))
      (var-exp (var)
        (nameless-var-exp
          (apply-senv senv var)))
      (let-exp (var exp1 body)
        (nameless-let-exp
          (translation-of exp1 senv)
          (translation-of body (extend-senv var senv))))
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body
            (extend-senv var senv))))
      (call-exp (rator rand)
        (call-exp
          (translation-of rator senv)
          (translation-of rand senv)))
      (else
        (eopl:error "unknow type ~s" exp)))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
        (num-val
          (- (expval->num (value-of exp1 nameless-env))
             (expval->num (value-of exp2 nameless-env)))))
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 nameless-env)))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 nameless-env))
            (value-of exp2 nameless-env)
            (value-of exp3 nameless-env)))
      (call-exp (rator rand)
        (let ((proc (expval->proc (value-of rator nameless-env)))
              (arg (value-of rand nameless-env)))
            (apply-procedure proc arg)))
      (nameless-var-exp (idx)
        (apply-nameless-env nameless-env idx))
      (nameless-let-exp (exp1 body)
        (let ((val (value-of exp1 nameless-env)))
             (value-of body (extend-nameless-env val nameless-env))))
      (nameless-proc-exp (body)
        (proc-val (procedure body nameless-env)))
      (else eopl:error "unknow expression type ~s" exp))))

(define run
  (lambda (str)
    (value-of-program
      (a-program
        (translation-of-program
          (let-scan-parse str))))))

(define init-nameless-env
  (lambda ()
    (extend-nameless-env
      (num-val 1)
      (extend-nameless-env
        (num-val 2)
        (extend-nameless-env
          (num-val 3)
          (empty-nameless-env))))))

(define value-of-program
  (lambda (pg)
    (cases program pg
      (a-program (exp1)
        (value-of exp1 (init-nameless-env))))))

(eopl:pretty-print
  (run "let fn = proc (x) x in -((fn 3),2)"))
