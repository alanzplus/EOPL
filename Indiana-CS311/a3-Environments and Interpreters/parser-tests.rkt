#lang racket

(require rackunit "a3.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "Parser:"
              (test-equal? "num-expr"
                           (parse '3)
                           (num-expr 3))
              (test-equal? "var-expr"
                           (parse 'a)
                           (var-expr 'a))
              (test-equal? "boolean-expr"
                           (parse '#t)
                           (boolean-expr #t))
              (test-equal? "simple-lambda-expr"
                           (parse '(lambda (id) id))
                           (lambda-expr 'id (var-expr 'id)))
              (test-equal? "let-expr"
                           (parse '(let ([var1 'x] [var2 '(lambda (x) (* x 3))]) (var2 var1)))
                           (let-expr
                             (list
                               (binding-expr 'var1 (call-expr (var-expr 'quote) (var-expr 'x)))
                               (binding-expr
                                 'var2
                                 (call-expr
                                   (var-expr 'quote)
                                   (lambda-expr 'x (mul-expr (var-expr 'x) (num-expr 3))))))
                             (call-expr (var-expr 'var2) (var-expr 'var1))))
              (test-equal? "sub1-expr"
                           (parse '(sub1 (sub1 a)))
                           (sub1-expr (sub1-expr (var-expr 'a))))
              (test-equal? "mul-expr"
                           (parse '(* a b))
                           (mul-expr (var-expr 'a) (var-expr 'b)))
              (test-equal? "if-expr"
                           (parse '(if #t 3 (lambda (x) x)))
                           (if-expr (boolean-expr #t) (num-expr 3) (lambda-expr 'x (var-expr 'x))))
              (test-equal? "zero?-expr"
                           (parse '(zero? 0))
                           (zero?-expr (num-expr 0)))
              (test-equal? "call-expr"
                           (parse '(a b))
                           (call-expr (var-expr 'a) (var-expr 'b)))
))

(run-tests tests)
