#lang racket
;; Written for Spring 2013 by Andre Kuhlenschmidt and Jason Hemann
;; Modified for Spring 2015 by Jason Hemann

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
  (lambda (#:file-name (file "./a7.rkt")
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
some examples of how test might be structured. If further documentation is
require feel free to browse the rackunit documentation at the following address.
http://docs.racket-lang.org/rackunit/?q=rackunit
|#

(define tests
  (test-suite "a7"

    (test-suite "last-non-zero"
      (test-equal-if-defined last-non-zero
        ((last-non-zero '(0)) '())
        ((last-non-zero '(1 2 3 0 4 5)) '(4 5))
        ((last-non-zero '(1 0 2 3 0 4 5)) '(4 5))
        ((last-non-zero '(1 2 3 4 5)) '(1 2 3 4 5))))

    (test-suite "lex" 
      (test-equal-if-defined lex
        ((lex '(lambda (x) x) '())
          '(lambda (var 0)))
        ((lex '(lambda (x) 120) '())
          '(lambda (const 120)))
        ((lex '(lambda (y) (lambda (x) y)) '())
         '(lambda (lambda (var 1))))
        ((lex '((lambda (x) x) 5) '())
         '(app (lambda (var 0)) (const 5)))
        ((lex '((lambda (x) 0) 5) '())
         '(app (lambda (const 0)) (const 5)))
        ((lex '(lambda (y) (lambda (x) (x y))) '())
         '(lambda (lambda (app (var 0) (var 1)))))
        ((lex '(lambda (x) (lambda (x) (x x))) '())
         '(lambda (lambda (app (var 0) (var 0)))))
        ((lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
         '(lambda (app (lambda (app (var 0) (var 1))) (lambda (lambda (app (var 2) (var 1)))))))
        ((lex '(let/cc k 5) '()) '(letcc (const 5)))
        ((lex '(let/cc k (throw k 5)) '()) '(letcc (throw (var 0) (const 5))))
        ((lex '(let/cc k (throw k (* 5 5))) '()) '(letcc (throw (var 0) (mult (const 5) (const 5)))))
        ((lex '(let/cc k (throw k (((lambda (x) x) k) (* 5 5)))) '()) 
         '(letcc
           (throw
            (var 0)
            (app (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5))))))
        ((lex '(let/cc k (sub1 (throw k 5))) '()) '(letcc (sub1 (throw (var 0) (const 5)))))
        ((lex '(let/cc k (throw (throw k 5) 6)) '()) 
         '(letcc (throw (throw (var 0) (const 5)) (const 6))))
        ((lex '(let/cc k (throw 5 (throw k 5))) '())
         '(letcc (throw (const 5) (throw (var 0) (const 5)))))
        ((lex '(* 3 (let/cc k (throw 5 (throw k 5)))) '())
         '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))))
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
                          (app (app (app (app (app (var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
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
                      (app (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                       (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))
        ((lex '((lambda (x) x) 5) '())
         '(app (lambda (var 0)) (const 5)))
        ((lex '(lambda (!)
                 (lambda (n)
                   (if (zero? n) 1 (* n (! (sub1 n))))))
              '())
         '(lambda
            (lambda
              (if (zero (var 0))
                  (const 1)
                  (mult (var 0) (app (var 1) (sub1 (var 0))))))))
        ((lex
          '(let ((! (lambda (!)
                      (lambda (n)
                        (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
             ((! !) 5))
          '())
         '(let (lambda
                 (lambda
                   (if (zero (var 0))
                       (const 1)
                       (mult (var 0) (app (app (var 1) (var 1)) (sub1 (var 0)))))))
            (app (app (var 0) (var 0)) (const 5))))))


    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
        ((value-of-cps '(const 5) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
        ((value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
        ((value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
        ((value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
        ((value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
        ((value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5) 
        ((value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
        ((value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
        ((value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
        ((value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        ((value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        ((value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
        ((value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                       (empty-env)
                       (empty-k))
         4)
        ((value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                       (empty-env)
                       (empty-k))
         4)
        ((value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                             (lambda
                               (lambda 
                                 (if (zero (var 0))  
                                     (const 1)
                                     (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                       (empty-env)
                       (empty-k))
         1)))
    
    (test-suite "trib$"
      (test-equal-if-defined trib$
	((car$ trib$) 0)
	((car$ (cdr$ trib$)) 1)
	((take$ 7 trib$) '(0 1 1 2 4 7 13))))))

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

(test-file #:file-name "a7.rkt")
