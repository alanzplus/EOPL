#lang eopl

(require "../parser/let-spec.rkt")

; program ::= expression
; expression ::= number
; expression ::= -(expression, expression)
; expression ::= zero? (expression)
; expression ::= if expression then expression else expression
; expression ::= identifier
; expression ::= let identifier = expression in expression
; TODO complete the specs

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
(provide expval->list)
(provide expval->proc)

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

(define index-of
  (lambda (alist ele)
    (index-of-aux alist ele 0)))

(define index-of-aux
  (lambda (alist ele idx)
    (if (null? alist)
        -1
        (if (eqv? (car alist) ele)
            idx
            (index-of-aux (cdr alist) ele (+ 1 idx))))))

(define get
  (lambda (alist idx)
    (if (eqv? idx 0)
        (car alist)
        (get (cdr alist) (- idx 1)))))

(define extend-env-rec
  (lambda (p-name-list b-var-list p-body-list saved-env)
    (letrec ((rec-env
              (lambda (search-var)
                (let ((idx (index-of p-name-list search-var)))
                     (if (eqv? idx -1)
                         (apply-env saved-env search-var)
                         (proc-val (procedure (get b-var-list idx) (get p-body-list idx) rec-env)))))))
            rec-env)))

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
  (lambda (var-list body env)
    (lambda (val-list)
      (value-of body (extend-env-list var-list val-list env)))))

(define extend-env-list
  (lambda (var-list val-list env)
    (if (and (null? var-list) (null? val-list))
        env
        (extend-env
          (car var-list)
          (car val-list)
          (extend-env-list (cdr var-list) (cdr val-list) env)))))

(define apply-procedure
  (lambda (proc1 val-list)
    (proc1 val-list)))

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

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (alist) alist)
      (empty-list-val () '())
      (else (eopl:error "expected list-val")))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (eopl:error "expected proc-val")))))

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
      (letrec-exp (p-name-list b-var-list p-body-list letrec-body)
        (value-of letrec-body (extend-env-rec p-name-list b-var-list p-body-list env)))
      (letproc-exp (p-name b-var-list p-body letproc-body)
        (value-of letproc-body (extend-env p-name (proc-val (procedure b-var-list p-body env)) env)))
      (call-exp (exp1 exp-list)
        (let ((proc (expval->proc (value-of exp1 env)))
              (args (map (lambda (arg) (value-of arg env)) exp-list)))
             (apply-procedure proc args)))
      (proc-exp (var-list body) (proc-val (procedure var-list body env)))
      (list-exp (exp-list)
        (list-val (map (lambda (ele) (value-of ele env)) exp-list)))
      (cons-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
             (list-val (list val1 val2))))
      (cdr-exp (exp1)
        (let ((val1 (value-of exp1 env)))
             (cases expval val1
              (list-val (alist) (list-val (cdr alist)))
              (empty-list-val () (eopl:error "list is empty"))
              (else (eopl:error "not a list")))))
      (car-exp (exp1)
        (let ((val1 (value-of exp1 env)))
             (cases expval val1
              (list-val (alist) (car alist))
              (empty-list-val () (eopl:error "list is empty"))
              (else (eopl:error "not a list")))))
      (null?-exp (exp1)
        (cases expval (value-of exp1 env)
          (list-val (alist) (bool-val (null? alist)))
          (empty-list-val () (bool-val #t))
          (else eopl:error "~s not evaluated to a list" exp1)))
      (emptylist-exp () (empty-list-val))
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