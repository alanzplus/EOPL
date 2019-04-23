#lang racket
;; C311 Assignment 3 test suite
;; Written in Spring 2010 by Lindsey Kuper
;; Updated for Spring 2012 by Ross Larson
;; Updated for Fall 2013 by Jason Hemann
;; Updated for Spring 2013 by Andre Kuhlenschmidt and Jason Hemann

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
  (lambda (#:file-name (file "./a3.rkt")
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
  (test-suite "A3"

   (test-suite "value-of"
   (test-equal-if-defined value-of
     ;; 0 booleans                          
     [(value-of
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     #t]
    ;; 1 if
    [(value-of
      '((lambda (x)
          (if (zero? x) 12 47))
                 0)
               (lambda (y) (error 'value-of "unbound variable ~s" y)))
     12]
    ;; 2 let
    [(value-of
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     60]
    ;; 3 let
    [(value-of
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     30]
    ;; 4 let
    [(value-of
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     25]
    ;; 5 Poor Man's Y using let - Fact
    [(value-of
      '(let ((! (lambda (x) (* x x))))
         (let ((! (lambda (n)
                    (if (zero? n)
                        1
                        (* n (! (sub1 n)))))))
           (! 5)))
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     80]
    ;; 6 Poor Man's Y using lambda - Fact
    [(value-of
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     120]))

   (test-suite "value-of-fn"
   (test-equal-if-defined value-of-fn
     ;; 0 booleans                          
     [(value-of-fn
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (empty-env-fn))
     #t]
    ;; 1 if                      
    [(value-of-fn
      '((lambda (x) (if (zero? x)
                   12
                   47))
        0)
      (empty-env-fn))
     12]
    ;; 2 let
    [(value-of-fn
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (empty-env-fn))
     60]
    ;; 3 let
    [(value-of-fn
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (empty-env-fn))
     30]
    ;; 4 let
    [(value-of-fn
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (empty-env-fn))
     25]
    ;; 5 Poor Man's Y using lambda - Fact
    [(value-of-fn
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (empty-env-fn))
     120]))

   (test-suite "value-of-ds"
   (test-equal-if-defined value-of-ds
    ;; 0 booleans
    [(value-of-ds
      '((lambda (x) (if (zero? x)
                   #t
                   #f))
        0)
      (empty-env-ds))
     #t]
    ;; if
    [(value-of-ds
      '((lambda (x) (if (zero? x)
                   12
                   47))
        0)
      (empty-env-ds))
     12]
    ;; 2 let
    [(value-of-ds
      '(let ([y (* 3 4)])
         ((lambda (x) (* x y)) (sub1 6)))
      (empty-env-ds))
     60]
    ;; 3 let
    [(value-of-ds
      '(let ([x (* 2 3)])
         (let ([y (sub1 x)])
           (* x y)))
      (empty-env-ds))
     30]
    ;; 4 let
    [(value-of-ds
      '(let ([x (* 2 3)])
         (let ([x (sub1 x)])
           (* x x)))
      (empty-env-ds))
     25]
    ;; 6 Poor Man's Y using lamda - Fact
    [(value-of-ds
      '(((lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
         (lambda (f)
           (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
        5)
      (empty-env-ds))
     120]))

   (ifdef-suite fo-eulav
    ;; 1
    [(fo-eulav '(5 (x (x) adbmal)) (empty-env))  5]
    ;; Stnemugra sa Snoitcnuf
    [(fo-eulav '(((x 1bus) (x) adbmal)
                ((5 f) (f) adbmal))
              (empty-env))
    4]
    ;; Tcaf
    [(fo-eulav   '(5
                   (((((((n 1bus) (f f)) n *)
                       1
                       (n ?orez) fi)
                      (n) adbmal)
                     (f) adbmal)
                    ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                      (n) adbmal)
                     (f) adbmal))) (empty-env))
    120])

   (test-suite "Brain teasers"
     (test-suite "Set!"
      (test-equal-if-defined value-of
     [(value-of
       '(* (begin2 1 1) 3)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of
       '((lambda (a)
           ((lambda (p)
              (begin2
               (p a)
               a)) (lambda (x) (set! x 4)))) 3)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of
       '((lambda (f)
           ((lambda (g)
              ((lambda (z) (begin2
                       (g z)
                       z))
               55))
            (lambda (y) (f y)))) (lambda (x) (set! x 44)))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      55]
     [(value-of
      '((lambda (x)
          (begin2 (set! x 5) x))
        6)
      (lambda (y) (error 'value-of "unbound variable ~s" y)))
     5]
     [(value-of
       '(let ((a 3))
          (begin2 (begin2 a (set! a 4)) a))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      4]
     [(value-of
       '((lambda (x)
           (begin2
            ((lambda (y)
               (begin2
                (set! x 0)
                98))
             99)
            x))
         97)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
     0]
     [(value-of
       '((lambda (y)
           (let ((x (begin2
                     (set! y 7)
                     8)))
             (begin2
              (set! y 3)
              ((lambda (z) y)
               x))))
         4)
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      3]
     [(value-of 
       '(let ((a 5))
          (let ((y (begin2 (set! a (sub1 a)) 6)))
            (begin2
              (* y y)
              a)))
       (lambda (y) (error 'value-of "unbound variable ~s" y)))
      4]))
     (test-suite "value-of-lex helpers"
      (test-equal-if-defined apply-env-lex
        ((value-of-lex '((lambda (var 0)) (const 5)) '())
         5)
        ((value-of-lex '(((lambda (lambda (var 1))) (const 5)) (const 6)) '())
         5)
        ((value-of-lex '((((lambda
                             (lambda
                               (lambda
                                 (if (zero (var 2))
                                     (var 1)
                                     (var 0)))))
                           (const 0))
                          (const 10))
                         (const 20))
                       '())
         10))))
   (test-suite "church numerals"
     (test-equal-if-defined csub1
       ((let ((c5 (lambda (f) (lambda (x) (f (f (f (f (f x)))))))))
          (((csub1 c5) add1) 0))
        4)
       ((let ((c0 (lambda (f) (lambda (x) x))))
          (((csub1 c0) add1) 0))
        0)))))

   
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


