#lang racket
;; Written for Spring 2013 by Andre Kuhlenschmidt and Jason Hemann
;; Modified Fall 2016 Jason Hemann

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
  (lambda (#:file-name (file "./a13.rkt")
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
  (test-suite "a13"
    (test-suite "listo"
      (test-equal-if-defined listo
    ((run 1 (q) (listo '(a b c d e))) '(_.0))
    ((run 1 (q) (listo '(a b c d . e))) '())
    ((run 4 (q) (listo q)) '(() (_.0) (_.0 _.1) (_.0 _.1 _.2)))
    ((run 4 (q) (listo `(a b ,q))) '(_.0))))
    (test-suite "facto"
      (test-equal-if-defined facto
    ((run 1 (q) (facto  q '(0 0 0 1 1 1 1))) '((1 0 1)))
    ((run 1 (q) (facto (build-num 5) q)) '((0 0 0 1 1 1 1)))
    ((run 6 (q) (fresh (n1 n2) (facto n1 n2) (== `(,n1 ,n2) q))) 
     '((() (1)) 
       ((1) (1)) 
       ((0 1) (0 1)) 
       ((1 1) (0 1 1))
       ((0 0 1) (0 0 0 1 1)) 
       ((1 0 1) (0 0 0 1 1 1 1))))))
    (test-suite "fibso"
      (test-equal-if-defined fibso
    ((run 4 (q) 
       (fresh (n o1 o2) 
         (== q `(,n ,o1 ,o2)) 
         (fibso n o1 o2))) 
     '((() (1) (1))
       ((1) (1) (0 1))
       ((0 1) (0 1) (1 1))
       ((1 1) (1 1) (1 0 1))))
    ((run 1 (q) 
       (fresh (n o1) 
         (== q `(,n ,o1))
         (fibso n o1 (build-num 5)))) '(((1 1) (1 1))))
    ((run 1 (q) 
       (fresh (n o2) 
         (== q `(,n ,o2))
         (fibso n (build-num 5) o2))) '(((0 0 1) (0 0 0 1))))))
    (test-suite "fo-lavo"
      (test-equal-if-defined fo-lavo
    ((run 1 (q) (fo-lavo q '() '() q))
     '(((((((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal)
          etouq)
         (((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal))
        (=/= ((_.0 closure)) ((_.0 etouq)) ((_.0 tsil)))
        (sym _.0))))
    ((run 3 (q) (fresh (a c d)
              (val-ofo `(,a ,d) '() '() c)
              (fo-lavo `(,c ,d) '() '() a)
              (== `(,a ,c ,d) q))) 
     '(((quote ('etouq (_.0) adbmal) ('etouq (_.0) adbmal))
        (=/= ((_.0 closure)) ((_.0 etouq)))
        (sym _.0))
       ((quote
         (((_.0 etouq) ('etouq (_.1) adbmal)) (_.2) adbmal)
         (((_.0 etouq) ('etouq (_.1) adbmal)) (_.2) adbmal))
        (=/= ((_.1 closure)) ((_.1 etouq)) ((_.2 adbmal))
         ((_.2 closure)) ((_.2 etouq)))
        (sym _.1 _.2)
        (absento (closure _.0)))
       ((quote
         ((_.0 etouq) (('etouq (_.1) adbmal) (_.2) adbmal))
         ((_.0 etouq) (('etouq (_.1) adbmal) (_.2) adbmal)))
        (=/= ((_.1 closure))
         ((_.1 etouq))
         ((_.2 adbmal))
         ((_.2 closure)))
        (sym _.1 _.2)
        (absento (closure _.0)))))))
    (test-suite "color-middle-earth"
      (test-equal-if-defined color-middle-earth
    ((color-middle-earth '(red orange purple black)) 
     '(((lindon . red) (forodwaith . orange) (eriador . purple) (rhovanion . red)
        (enedwaith . orange) (rohan . purple) (gondor . red)
        (rhun . orange) (mordor . black) (khand . red)
        (harad . orange))))))))

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


