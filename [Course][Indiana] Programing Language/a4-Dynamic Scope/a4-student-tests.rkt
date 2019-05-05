#lang racket
;; Written for Spring 2013 by Andre Kuhlenschmidt and Jason Hemann

#| The underlying principles of the autograding framework is simple. 
We use the rackunit unit testing framework that comes with the racket
distrobution. We have define a set of calls that take a test-suite.
and executes the test-suite with eval rebound to a sandboxed evaluator
that provides racket and the file being tested. Disk access should be
limited to those files and time and space are limited by the parameters
time-for-eval and space-for-eval. 
|#

(require rackunit rackunit/text-ui racket/sandbox wxme)
(provide test-file)

#|
 Test File is the minimum requirement for being able to understand
 our the autograder. test-file is a function that when invoked with
 no arguments will search will the current directory for the file 
 named a1.rkt and run the test suite with that file.
 If a single argument is provided that argument must be the relative or
 absolute path to the file containing the definitions for the assignment.
|#

(define test-file
  (lambda (#:file-name (file "./a4.rkt")
       #:sec-of-eval (sec 5)
       #:mb-of-eval (mb 5))
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((input-port (open-input-file file)))
        (if (is-wxme-stream? input-port)
            (error 'test-file "Your file contains non-text elements (e.g. pictures, comment boxes). Please remove them and retry")
            (let ((sandboxed-eval (make-module-evaluator (read input-port))))
              (set-eval-limits sandboxed-eval sec mb)
              (parameterize ((current-eval sandboxed-eval)
                             (error-print-context-length 0))
                (run-tests tests))))))))

#|
  A tests is the name of the test-suite that is run by a call to test-file.
The test suite is a type that is define in the rackunit module. I will give
some examples of how test might be structured. If furthure documentation is
require feel free to browse the rackunit documentation at the following address.
http://docs.racket-lang.org/rackunit/?q=rackunit
|#
    
(define tests
  (test-suite "a4"
    (test-suite "lex" 
      (test-equal-if-defined lex
        ((lex '(lambda (x) x) '())
          '(lambda (var 0)))
        ((lex '(lambda (y) (lambda (x) y)) '())
         '(lambda (lambda (var 1))))
        ((lex '(lambda (y) (lambda (x) (x y))) '())
         '(lambda (lambda ((var 0) (var 1)))))
        ((lex '(lambda (x) (lambda (x) (x x))) '())
         '(lambda (lambda ((var 0) (var 0)))))
        ((lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
         '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
        ((lex '(lambda (a)
                 (lambda (b)
                   (lambda (c)
                     (lambda (a)
                       (lambda (b)
                         (lambda (d)
                           (lambda (a)
                             (lambda (e)
                               (((((a b) c) d) e) a)))))))))
              '())
         '(lambda
            (lambda
              (lambda
                (lambda
                  (lambda
                    (lambda
                      (lambda
                        (lambda
                          ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
        ((lex '(lambda (a)
                 (lambda (b)
                   (lambda (c)
                     (lambda (w)
                       (lambda (x)
                         (lambda (y)
                           ((lambda (a)
                              (lambda (b)
                                (lambda (c)
                                  (((((a b) c) w) x) y))))
                            (lambda (w)
                              (lambda (x)
                                (lambda (y)
                                  (((((a b) c) w) x) y)))))))))))
              '())
         '(lambda 
            (lambda 
              (lambda 
                (lambda 
                  (lambda 
                    (lambda 
                      ((lambda
                         (lambda
                           (lambda
                             ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                       (lambda
                         (lambda
                           (lambda
                             ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))
        ((lex '((lambda (x) x) 5) '())
         '((lambda (var 0)) (const 5)))
        ((lex '(lambda (!)
                 (lambda (n)
                   (if (zero? n) 1 (* n (! (sub1 n))))))
              '())
         '(lambda
            (lambda
              (if (zero? (var 0))
                  (const 1)
                  (* (var 0) ((var 1) (sub1 (var 0))))))))
        ((lex
          '(let ((! (lambda (!)
                      (lambda (n)
                        (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
             ((! !) 5))
          '())
         '(let (lambda
                 (lambda
                   (if (zero? (var 0))
                       (const 1)
                       (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
            (((var 0) (var 0)) (const 5))))))          
    (test-suite "value-of/RI-closures/fn-reps"
      (test-equal-if-defined value-of-fn
        ((value-of-fn 
           '((lambda (x) (if (zero? x) 
                             12 
                             47)) 
              0) 
           (empty-env))
         12)    
        ((value-of-fn
          '(let ([y (* 3 4)])
             ((lambda (x) (* x y)) (sub1 6)))
          (empty-env))
         60)
        ((value-of-fn
          '(let ([x (* 2 3)])
             (let ([y (sub1 x)])
               (* x y)))
          (empty-env))
         30)
        ((value-of-fn
          '(let ([x (* 2 3)])
             (let ([x (sub1 x)])
               (* x x)))
          (empty-env))
         25)))
    (test-suite "value-of/RI-closures/ds-reps"
      (test-equal-if-defined value-of-ds
        ((value-of-ds
          '((lambda (x) (if (zero? x) 
                    12 
                    47)) 
            0) 
          (empty-env))
         12)    
        ((value-of-ds
          '(let ([y (* 3 4)])
             ((lambda (x) (* x y)) (sub1 6)))
          (empty-env))
         60)
        ((value-of-ds
          '(let ([x (* 2 3)])
             (let ([y (sub1 x)])
               (* x y)))
          (empty-env))
         30)
        ((value-of-ds
          '(let ([x (* 2 3)])
             (let ([x (sub1 x)])
               (* x x)))
          (empty-env))
         25)))
    (test-suite "value-of-dynamic"
      (test-equal-if-defined value-of-dynamic
        ((value-of-dynamic
          '(let ([x 2])
             (let ([f (lambda (e) x)])
               (let ([x 5])
                 (f 0))))
          (empty-env))
         '5)
        ((value-of-dynamic
          '(let ([! (lambda (n)
                      (if (zero? n) 
                          1
                          (* n (! (sub1 n)))))])
             (! 5))
          (empty-env))
         '120)
        ((value-of-dynamic
          '((lambda (!) (! 5))
            (lambda (n)
              (if (zero? n) 
                  1
                  (* n (! (sub1 n))))))
          (empty-env))
         120)
        ((value-of-dynamic
          '(let ([f (lambda (x) (cons x l))])
             (let ([cmap 
                    (lambda (f)
                      (lambda (l)               
                        (if (null? l) 
                            '()
                            (cons (f (car l)) ((cmap f) (cdr l))))))])
               ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
          (empty-env))
         '((1 1 2 3) (2 2 3) (3 3)))))
    (test-suite "value-of-ri"
      (test-equal-if-defined "value-of-ri"
        (((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
         5)
        (((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
         5)))))


(define-syntax test-if-defined
  (syntax-rules ()
    ((_ sym tests ...)
     (test-case (format "~a undefined" 'sym)
                (check-not-false (lambda () (eval 'sym)))
                tests ...))))

(define-syntax test-equal-if-defined
  (syntax-rules ()
    ((_ ident (expr val) ...)
      (let ((n 1))
        (test-case (format "~a: undefined" 'ident)
                   (check-not-exn (lambda () (eval 'ident)))
                   (test-case (format "~a: ~a" 'ident n)
                              (with-check-info 
                               (('tested 'expr))
                               (set! n (add1 n))
                               (check equal? (eval 'expr) val))) ...)))))

(define-syntax ifdef-suite
  (syntax-rules ()
    ((_ ident (expr val) ...)
     (let ((n 1))
       (test-suite (~a 'ident)
        (test-case "undefined"
         (check-not-exn (lambda () (eval 'ident)))
         (test-case (~a n)
          (with-check-info (('tested 'expr))
           (set! n (add1 n))
           (check equal? (eval 'expr) val))) ...))))))

(test-file #:file-name "a4.rkt")
