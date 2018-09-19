#lang eopl

(require "../parser/let-spec.rkt")
(require "../common/env.rkt")

; program ::= expression
; expression ::= number
; expression ::= -(expression, expression)
; expression ::= zero? (expression)
; expression ::= if expression then expression else expression
; expression ::= identifier
; expression ::= let identifier = expression in expression

(provide run)
(provide num-val)
(provide bool-val)
(provide expval->num)
(provide expval->bool)

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?)))

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

(define run
  (lambda (text)
    (value-of-program (let-scan-parse text))))

(define init-env
  (lambda ()
    (extend-env
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
          (extend-env
            'x (num-val 10)
            (empty-env))))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env var env))
      (diff-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
              (num-val (- num1 num2))))
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env)))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
            (value-of exp2 env)
            (value-of exp3 env)))
      (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
               (value-of body (extend-env var val1 env)))))))
