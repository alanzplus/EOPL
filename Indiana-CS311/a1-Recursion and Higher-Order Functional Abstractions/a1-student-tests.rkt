#lang racket
;; C311 Assignment 1 test suite
;; Written in Spring 2010 by Lindsey Kuper
;; Updated for Spring 2012 by Ross Larson
;; Updated for Fall 2013 by Jason Hemann
;; Updated for Spring 2014 by Andre Kuhlenschmidt
;; Updated for Fall 2014 by Jason Hemann
;; Updated for Spring 2015 by Jason Hemann
;; Updated for Fall 2018 by Paulette Koronkevich

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
  (lambda (#:file-name (file "./a1.rkt")
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
  (test-suite "A1:"
    (test-suite "countdown"
      (test-equal-if-defined countdown
        ((countdown 5) '(5 4 3 2 1 0))))
    (test-suite "insertR"
      (test-equal-if-defined insertR
        ((insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y))))
   ;; As with the original you may put as many tests as you would
   ;; like aftwards.
   (test-suite "insertR-fr" 
    (test-equal-if-defined insertR-fr
      ((insertR-fr 'x 'y '(x z z x y x)) '(x y z z x y y x y))
      ((insertR-fr 'x 'y '(x x x)) '(x y x y x y))))
   
   ;; And the most syntax sugar that I am going to offer test-equal-if-defined 
   ;; test-equal-if-defined is a macro expanding to check to see if this procedure
   ;; exists in the context of eval. If so the list following is tested for
   ;; equality the leftmost being tested with the sandboxed eval function.
   (test-suite "remv-1st"
     (test-equal-if-defined remv-1st
       [(remv-1st 'x '(x y z x)) '(y z x)]
       [(remv-1st 'y '(x y z y x)) '(x z y x)]))

   (test-suite "list-index-ofv?"
     (test-equal-if-defined list-index-ofv? 
       ((list-index-ofv? 'x '(x y z x x)) 0)
       ((list-index-ofv? 'x '(y z x x)) 2)))

   (test-suite "filter"   
     (test-equal-if-defined filter
       [(filter even? '(1 2 3 4 5 6)) '(2 4 6)]))

   (test-suite "filter-fr"   
     (test-equal-if-defined filter-fr
       [(filter-fr even? '(1 2 3 4 5 6)) '(2 4 6)]))

   (test-suite "zip"   
     (test-equal-if-defined zip
       [(zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))]
       [(zip '(1 2 3 4 5) '(a b c)) '((1 . a) (2 . b) (3 . c))]
       [(zip '(1 2 3) '(a b c d e)) '((1 . a) (2 . b) (3 . c))]))

   (test-suite "fact"   
     (test-equal-if-defined fact
       [(fact 0) 1]                     
       [(fact 5) 120]))

   (test-suite "map"   
     (test-equal-if-defined map
       [(map add1 '(1 2 3 4)) '(2 3 4 5)]))

   (test-suite "map-fr"   
     (test-equal-if-defined map-fr
       [(map-fr add1 '(1 2 3 4)) '(2 3 4 5)]))

   (test-suite "append"   
     (test-equal-if-defined append
       [(append '(a b c) '(1 2 3)) '(a b c 1 2 3)]))

   (test-suite "append-fr"   
     (test-equal-if-defined append-fr
       [(append-fr '(a b c) '(1 2 3)) '(a b c 1 2 3)]))

   (test-suite "reverse"   
     (test-equal-if-defined reverse
       [(reverse '(a 3 x)) '(x 3 a)]))

   (test-suite "reverse-fr"   
     (test-equal-if-defined reverse-fr
       [(reverse-fr '(a 3 x)) '(x 3 a)]))

   (test-suite "memv"   
     (test-equal-if-defined memv
       [(memv 'a '(a b c)) '(a b c)]
       [(memv 'b '(a ? c)) #f]
       [(memv 'b '(a b c b)) '(b c b)]))

   (test-suite "fib"   
     (test-equal-if-defined fib
       [(fib 0) 0]
       [(fib 1) 1]
       [(fib 7) 13]))
   
   (test-suite "append-map"
     (test-equal-if-defined append-map
       [(append-map countdown (countdown 5))
        '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)]))
   
   (test-suite "append-map-fr"
     (test-equal-if-defined append-map-fr
       [(append-map-fr countdown (countdown 5))
        '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)]))   
   
   (test-suite "set-difference"   
     (test-equal-if-defined set-difference
       [(set-difference '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5)]))

   (test-suite "set-difference-fr"   
     (test-equal-if-defined set-difference-fr
       [(set-difference-fr '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5)]))
         
   (test-suite "powerset"   
     (test-equal-if-defined powerset
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset '())) <-set))
        '(())]
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset '(2 0 1))) <-set))
        '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ())]
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset '(1 3 4 5))) <-set))
        '((5 4 3 1)
          (5 4 3)
          (5 4 1)
          (5 4)
          (5 3 1)
          (5 3)
          (5 1)
          (5)
          (4 3 1)
          (4 3)
          (4 1)
          (4)
          (3 1)
          (3)
          (1)
          ())]))
   
   (test-suite "powerset-fr"   
     (test-equal-if-defined powerset-fr
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset-fr '())) <-set))
        '(())]
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset-fr '(2 0 1))) <-set))
        '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ())]
       [(letrec
          ((<-set
            (lambda (s1 s2)
              (cond
               ((null? s1) #f)
               ((null? s2))
               ((< (car s1) (car s2)) #f)
               ((< (car s2) (car s1)))
               (else (<-set (cdr s1) (cdr s2)))))))
          (sort (map (lambda (s) (sort s >)) (powerset-fr '(1 3 4 5))) <-set))
        '((5 4 3 1)
          (5 4 3)
          (5 4 1)
          (5 4)
          (5 3 1)
          (5 3)
          (5 1)
          (5)
          (4 3 1)
          (4 3)
          (4 1)
          (4)
          (3 1)
          (3)
          (1)
          ())]))   
   
   (test-suite "cartesian-product"   
     (test-equal-if-defined cartesian-product
       [(letrec
          ((>=-tuple
        (lambda (t1 t2)
          (cond
           ((null? t1))
           ((> (car t1) (car t2)))
           ((> (car t2) (car t1)) #f)
           (else (>=-tuple (cdr t1) (cdr t2)))))))
          (sort (map (lambda (s) (sort s >)) (cartesian-product '((7 6 5) (3 2)))) >=-tuple))
        '((7 3) (7 2) (6 3) (6 2) (5 3) (5 2))]))
   
   (test-suite "cartesian-product-fr"   
     (test-equal-if-defined cartesian-product-fr
       [(letrec
          ((>=-tuple
        (lambda (t1 t2)
          (cond
           ((null? t1))
           ((> (car t1) (car t2)))
           ((> (car t2) (car t1)) #f)
           (else (>=-tuple (cdr t1) (cdr t2)))))))
          (sort (map (lambda (s) (sort s >)) (cartesian-product-fr '((7 6 5) (3 2)))) >=-tuple))
        '((7 3) (7 2) (6 3) (6 2) (5 3) (5 2))]))
   
   (test-suite "binary->natural"   
     (test-equal-if-defined binary->natural
       [(binary->natural '()) 0]
       [(binary->natural '(0 0 1)) 4]
       [(binary->natural '(0 0 1 1)) 12]
       [(binary->natural '(1 1 1 1)) 15]
       [(binary->natural '(1 0 1 0 1)) 21]
       [(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191]))

   (test-suite "binary->natural-fr"   
     (test-equal-if-defined binary->natural-fr
       [(binary->natural-fr '()) 0]
       [(binary->natural-fr '(0 0 1)) 4]
       [(binary->natural-fr '(0 0 1 1)) 12]
       [(binary->natural-fr '(1 1 1 1)) 15]
       [(binary->natural-fr '(1 0 1 0 1)) 21]
       [(binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191]))

   (test-suite "minus" 
     (test-equal-if-defined minus
       [(minus 5 3) 2]
       [(minus 100 50) 50]))

   (test-suite "div"   
     (test-equal-if-defined div
       [(div 25 5) 5]
       [(div 36 6) 6]))

   (test-suite "collatz"   
     (test-equal-if-defined collatz
       [(collatz 12) 1]
       [(collatz 120) 1]
       [(collatz 9999) 1]))

   (test-suite "quine"   
     (test-equal-if-defined quine
       [(equal? (eval quine) quine) #t]
       [(equal? (eval quine) (eval (eval quine))) #t]))))

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

(test-file #:file-name "a1.rkt")
