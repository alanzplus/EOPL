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
(require "monads.rkt")
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
  (lambda (#:file-name (file "./a13.rkt")
       #:sec-of-eval (sec 5)
       #:mb-of-eval (mb 5))
    (run-tests tests)
    #;
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((sandboxed-eval
             (make-module-evaluator (read (open-input-file file)))))
        (set-eval-limits sandboxed-eval sec mb)
        (parameterize ((current-eval sandboxed-eval)
                   (error-print-context-length 0))
          (run-tests tests))))))


#|
  A tests is the name of the test-suite that is run by a call to test-file.
The test suite is a type that is define in the rackunit module. I will give
some examples of how test might be structured. If furthure documentation is
require feel free to browse the rackunit documentation at the following address.
http://docs.racket-lang.org/rackunit/?q=rackunit
|#

(define tests
  (test-suite "a13"
    (test-suite "findf-maybe"
      (test-equal-if-defined findf-maybe
        ((findf-maybe symbol? '(1 2 c)) (Just 'c))
        ((findf-maybe boolean? '(#f 1 2 c)) (Just #f))
    ((findf-maybe number? '(a b c)) (Nothing))))
    (test-suite "partition-writer"
      (test-equal-if-defined partition-writer
                             ((run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))
     '((1 3 5 7 9) . (2 4 6 8 10)))
                             ((run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
     '((2 4 6 8 10) . (1 3 5 7 9)))))
    (test-suite "powerXpartials"
      (test-equal-if-defined powerXpartials
                             ((run-writer (powerXpartials 2 6)) '((2 4 8) . 64 ))
                             ((run-writer (powerXpartials 3 5)) '((3 9 81) . 243))
                             ((run-writer (powerXpartials 5 7)) '((5 25 125 15625) . 78125))))
    (test-suite "replace-with-count"
      (test-equal-if-defined replace-with-count
                             (((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
    '(4 . (a 0 (t 1 (e 2 t ((n . m) . 3) . f) . t) . r)))
                             (((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
'(3 ((h (i s . 0) . a) 1 s 2 e . n) . m))
                             (((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)
    '(6 . (1 (h (2 s . 3) . 4) . 5)))))
    (test-suite "reciprocal"
      (test-equal-if-defined reciprocal                   
        ((reciprocal 0) (Nothing))
    ((reciprocal 2) (Just 1/2))
    ((traverse-reciprocal '((1 . 2) . (3 . (4 . 5)))) 
     (Just '((1 . 1/2) . (1/3 . (1/4 . 1/5)))))
    ((traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
     (Nothing))))
    (test-suite "halve"
      (test-equal-if-defined halve
                             ((run-writer (halve 6)) '(() . 3))
                             ((run-writer (halve 5)) '((5) . 5))
    ((run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))
     '((1 3 5) . ((1 . 1) . (3 . (2 . 5)))))))
    (test-suite "state/sum"
      (test-equal-if-defined state/sum
                             (((run-state (state/sum 5)) 0) '(5 . 0))
                             (((run-state (state/sum 2)) 0) '(2 . 0))
                             (((run-state (state/sum 2)) 3) '(5 . 3))
    (((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)
     '(15 . ((0 . 1) 3 6 . 10)))))
    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
                             (((run-cont (value-of-cps 
                                            '((lambda (f)
                                                ((f f) 5))
                                              (lambda (f)
                                                (lambda (n)
                                                  (if (zero? n)
                                                      1
                                                      (* n ((f f) (sub1 n)))))))
                                            (empty-env)))
          (lambda (v) v))
         '120) 
                             (((run-cont (value-of-cps 
                                            '(* 3 (capture q (* 2 (return q 4)))) 
                                            (empty-env)))
          (lambda (v) v))
         '12)))
    #;
    (test-suite "same-fringe"
      (test-equal-if-defined yield-cont
    ((driver '(("Time" . "flies") . ("like" . ("an" . "arrow")))
         '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
     '((("time" . "FLIES") . ("like" . ("an" . "aRrOw")))
       ("Time" . ("flies" . (("like" . "an") . "arrow")))))
    ((driver '(("Time" . "flies") . ("like" . ("arrow" . "an")))
         '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
     '#f)))))


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



