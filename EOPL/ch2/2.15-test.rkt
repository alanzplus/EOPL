#lang eopl

(require rackunit "2.15.rkt")
(require rackunit/text-ui)

(define var-exp-1
  (var-exp 1))

(define var-exp-2
  (var-exp 2))

(define lc-exp-1
  (lambda-exp 'a var-exp-1))

(define lc-exp-2
  (lambda-exp 'b var-exp-2))

(define app-exp-1
  (app-exp lc-exp-1 lc-exp-2))

(define lambda-calculus-exp-test
  (test-suite
    "Tests for lambda-calculus expression"
    (check-equal? (var-exp? var-exp-1) #t)
    (check-equal? (var-exp? lc-exp-1) #f)
    (check-equal? (var-exp->var var-exp-1) 1)
    (check-equal? (lambda-exp? lc-exp-1) #t)
    (check-equal? (lambda-exp? var-exp-1) #f)
    (check-equal? (lambda-exp->bound-var lc-exp-1) 'a)
    (check-equal? (var-exp? (lambda-exp->body lc-exp-1)) #t)
    (check-equal? (app-exp? app-exp-1) #t)
    (check-equal?
      (lambda-exp->bound-var (app-exp->rator app-exp-1)) 'a)
    (check-equal?
      (lambda-exp->bound-var (app-exp->rand app-exp-1)) 'b)))

(run-tests lambda-calculus-exp-test)
