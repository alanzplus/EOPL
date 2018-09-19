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

(define arithmetic
  (lambda (operator exp1 exp2 env)
    (let ((num1 (expval->num (value-of exp1 env)))
          (num2 (expval->num (value-of exp2 env))))
         (num-val (operator num1 num2)))))

(define predicate
  (lambda (operator exp1 exp2 env)
    (let ((num1 (expval->num (value-of exp1 env)))
          (num2 (expval->num (value-of exp2 env))))
         (bool-val (operator num1 num2)))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (minus-exp (exp1) (num-val (- (expval->num (value-of exp1 env)))))
      (add-exp (exp1 exp2) (arithmetic + exp1 exp2 env))
      (diff-exp (exp1 exp2) (arithmetic - exp1 exp2 env))
      (mul-exp (exp1 exp2) (arithmetic * exp1 exp2 env))
      (div-exp (exp1 exp2) (arithmetic (lambda (e1 e2) (floor (/ e1 e2))) exp1 exp2 env))
      (equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
              (cases expval val1
                (bool-val (bool1)
                  (cases expval val2
                    (bool-val (bool2)
                      (bool-val (eqv? bool1 bool2)))
                    (else (bool-val #f))))
                (num-val (num1)
                  (cases expval val2
                    (num-val (num2)
                      (bool-val (eqv? num1 num2)))
                    (else (bool-val #f))))
                (else (bool-val #f)))))
      (greater?-exp (exp1 exp2) (predicate > exp1 exp2 env))
      (less?-exp (exp1 exp2) (predicate < exp1 exp2 env))
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env var env))
      (zero?-exp (exp1)
        (bool-val (zero? (expval->num (value-of exp1 env)))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
            (value-of exp2 env)
            (value-of exp3 env)))
      (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
               (value-of body (extend-env var val1 env)))))))
