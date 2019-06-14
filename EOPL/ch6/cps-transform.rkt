
#lang eopl

(require "../libs/common.rkt")
(require "cps-in-spec.rkt")
(require "cps-out-spec.rkt")

(provide cps-of-program)

(define global-variable-id 0)

; Expression -> TfExpression
(define cps-of-program
  (lambda (pgm)
    (set! global-variable-id 0)
    (cases Program pgm
           (a-program (exp1)
                      (cps-a-program
                        (cps-of-exps (list exp1)
                                     (lambda (new-args)
                                       (simple-exp->exp (car new-args)))))))))

; ListOf(Expression) x (ListOf(SimpleExpression) -> TfExpression) -> TfExpression
; A builder which taks a list of Simple Expression and constructs a Tail Form Expression
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ([exps exps])
      (let ((pos (list-index
                   (lambda (exp)
                     (not (expression-simple? exp)))
                   exps)))
        (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'var)))
            (cps-of-exp
              (list-ref exps pos)
              (cps-proc-exp (list var)
                            (cps-of-rest
                              (list-set exps pos (var-exp var)))))))))))

; Expression -> SimpleExpression
; Assumes (expression-simple? exp)
(define cps-of-simple-exp
  (lambda (exp)
    (cases Expression exp
           (var-exp (id) (cps-var-exp id))
           (const-exp (num) (cps-const-exp num))
           (diff-exp (exp1 exp2)
                     (cps-diff-exp
                       (cps-of-simple-exp exp1)
                       (cps-of-simple-exp exp2)))
           (zero?-exp (exp1)
                      (cps-zero?-exp (cps-of-simple-exp exp1)))
           (proc-exp (ids body)
                     (cps-proc-exp (append ids (list 'k%00))
                                   (cps-of-exp body (cps-var-exp 'k%00))))
           (else
             (eopl:error "invliad expression to cps-of-simple-exp ~s" exp)))))

; Expression x SimpleExpression -> TfExpression
; cont-exp is a simple expression, when it is evaludated, it is a continutation in the form of (lambda (x) ...)
(define cps-of-exp
  (lambda (exp cont-exp)
    (cases Expression exp
           (var-exp (id)
                    (to-apply-cont-form cont-exp (cps-var-exp id)))
           (const-exp (num)
                      (to-apply-cont-form cont-exp (cps-const-exp num)))
           (diff-exp (exp1 exp2)
                     (cps-of-diff-exp exp1 exp2 cont-exp))
           (zero?-exp (exp1)
                      (cps-of-zero? exp1 cont-exp))
           (proc-exp (vars body)
                     (to-apply-cont-form
                       cont-exp
                       (cps-proc-exp (append vars 'k%00)
                                     (cps-of-exp body (cps-var-exp 'k%00)))))
           (let-exp (var exp1 body)
                    (cps-of-let-exp var exp1 body cont-exp))
           (letrec-exp (p-names p-vars p-bodys body)
                       (cps-of-letrec-exp p-names p-vars p-bodys body cont-exp))
           (if-exp (exp1 exp2 exp3)
                   (cps-of-if-exp exp1 exp2 exp3 cont-exp))
           (call-exp (rator rands)
                     (cps-of-call rator rands cont-exp))
           (else (eopl:error "invalid expression to cps-of-exp ~s" exp)))))

(define cps-of-call
  (lambda (rator rands cont)
    (cps-of-exps (cons rator rands)
                 (lambda (simples)
                   (cps-call-exp
                     (car simples)
                     (append (cdr simples) (list cont)))))))

(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 cont)
    (cps-of-exps (list exp1)
                 (lambda (simples)
                   (cps-if-exp
                     (car simples)
                     (cps-of-exp exp2 cont)
                     (cps-of-exp exp3 cont))))))

; Listof(Vars) x Listof(Listof(Var)) x Listof(Expression) x SimpleExpression -> TfExpression
(define cps-of-letrec-exp
  (lambda (p-names p-varss p-bodies body cont)
    (cps-letrec-exp
      p-names
      (map
        (lambda (p-vars) (append p-vars (list 'k%00)))
        p-varss)
      (map
        (lambda (p-body)
          (cps-of-exp p-body (cps-var-exp 'k%00)))
        p-bodies)
      (cps-of-exp body cont))))

; Variable x Expression x Expression x SimpleExpression -> TfExpression
(define cps-of-let-exp
  (lambda (id exp1 body cont)
    (cps-of-exps (list exp1)
                 (lambda (simples)
                   (cps-let-exp
                     id
                     (car simples)
                     (cps-of-exp body cont))))))

; Expression x Expression x SimpleExpression -> TfExpression
(define cps-of-diff-exp
  (lambda (exp1 exp2 cont)
    (cps-of-exps (list exp1 exp2)
                 (lambda (simples)
                   (to-apply-cont-form
                     cont
                     (cps-diff-exp
                       (car simples)
                       (cadr simples)))))))

; Expression x SimleExpression -> TfExpression
(define cps-of-zero?
  (lambda (exp1 cont)
    (cps-of-exps (list exp1)
                 (lambda (simples)
                   (to-apply-cont-form
                     cont
                     (cps-zero?-exp (car simples)))))))

; Expression -> Bool
(define expression-simple?
  (lambda (exp)
    (cases Expression exp
           (const-exp (num) #t)
           (var-exp (var) #t)
           (diff-exp (exp1 exp2)
                     (and (expression-simple? exp1) (expression-simple? exp2)))
           (zero?-exp (exp1) (expression-simple? exp1))
           (proc-exp (ids exp1) #t)
           (else #f))))

; Given a prefix and append an globally unique id to the prefix
(define fresh-identifier
    (lambda (identifier)
      (set! global-variable-id (+ global-variable-id 1))
      (string->symbol
        (string-append
          (symbol->string identifier)
          "%"
          (number->string global-variable-id)))))

(define list-set
    (lambda (lst n val)
      (cond
        ((null? lst) (eopl:error 'list-set "ran off end"))
        ((zero? n) (cons val (cdr lst)))
        (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))

(define to-apply-cont-form
  (lambda (k-exp simple-exp)
    (cps-call-exp k-exp (list simple-exp))))
