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
  (lambda (#:file-name (file "./a11.rkt")
	   #:sec-of-eval (sec 5)
	   #:mb-of-eval (mb 5))
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((input-port (open-input-file file)))
        (if (is-wxme-stream? input-port)
            (error 'test-file "Your file contains non-text elements (e.g. pictures, comment boxes). Please remove them and retry")
            (let ((sandboxed-eval (make-module-evaluator (read input-port)
                                                         #:allow-for-require '("./mk.rkt" "./numbers.rkt"))))
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
  (test-suite "a11"
    (test-suite "!-"
      (test-equal-if-defined !-
        ((run* (q) (!- '() #t q)) '(Bool))
	((run* (q) (!- '() 17 q)) '(Nat))
	((run* (q) (!- '() '(zero? 24) q)) '(Bool))
	((run* (q) (!- '() '(zero? (sub1 24)) q)) '(Bool))
        ((run* (q) (!- '() '(not (zero? (sub1 24))) q)) '(Bool))
	((run* (q)
	   (!- '() '(zero? (sub1 (sub1 18))) q)) '(Bool))
	((run* (q)
	   (!- '()  '(lambda (n) (if (zero? n) n n)) q)) '((Nat -> Nat)))
	((run* (q)
	   (!- '() '((lambda (n) (zero? n)) 5) q)) '(Bool))
	((run* (q)
	   (!- '() '(if (zero? 24) 3 4) q)) '(Nat))
	((run* (q)
	   (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q)) '(Bool))
	((run* (q)
	   (!- '() '(lambda (x) (sub1 x)) q)) '((Nat -> Nat)))
	((run* (q)
	   (!- '() '(lambda (a) (lambda (x) (+ a x))) q)) '((Nat -> (Nat -> Nat))))
	((run* (q)
	   (!- '() '(lambda (f)
		      (lambda (x)
			((f x) x)))
	       q)) '(((_.0 -> (_.0 -> _.1)) -> (_.0 -> _.1))))
	((run* (q)
	   (!- '() '(sub1 (sub1 (sub1 6))) q)) '(Nat))
	((run 1 (q)
	   (fresh (t)
	     (!- '() '(lambda (f) (f f)) t))) '())
	((length (run 20 (q)
		   (fresh (lam a b)
		     (!- '() `((,lam (,a) ,b) 5) 'Nat)
		     (== `(,lam (,a) ,b) q)))) '20)
	((length (run 30 (q) (!- '() q 'Nat))) '30)
	((length (run 30 (q) (!- '() q '(Nat -> Nat)))) '30)
	((length (run 500 (q) (!- '() q '(Nat -> Nat)))) '500)    
	((length (run 30 (q) (!- '() q '(Bool -> Nat)))) '30)
	((length (run 30 (q) (!- '() q '(Nat -> (Nat -> Nat))))) '30)
	((length (run 100 (q)
		   (fresh (e t)
		     (!- '() e t)
		     (== `(,e ,t) q)))) '100)
	((length (run 100 (q)
		   (fresh (g e t)
		     (!- g e t)
		     (== `(,g ,e ,t) q)))) '100)
	((length
	  (run 100 (q)
	    (fresh (g v)
	      (!- g `(var ,v) 'Nat)
	      (== `(,g ,v) q)))) '100)
	((run 1 (q)
	   (fresh (g)
	     (!- g
		 '((fix (lambda (!)
			  (lambda (n)
			    (if (zero? n)
				1
				(* n (! (sub1 n)))))))
		   5)
		 q))) '(Nat))
	((run 1 (q)
	   (fresh (g)
	     (!- g
		 '((fix (lambda (!)
			  (lambda (n)
			    (* n (! (sub1 n))))))
		   5)
		 q))) '(Nat))
	((run* (q) (!- '() '(cons (zero? 1) (zero? 0)) q)) '((pairof Bool Bool)))
	((run* (q) (!- '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q)) '((pairof Bool (pairof Bool Bool))))
	((run* (t) (!- '() '(lambda (x) (cons x x)) t)) '((_.0 -> (pairof _.0 _.0))))
	((run* (t) (!- '() '(lambda (x) (lambda (y) (cons (zero? x) (+ x y)))) t)) '((Nat -> (Nat -> (pairof Bool Nat)))))
	((run* (t) (!- '() '(lambda (x) (zero? (car x))) t)) '(((pairof Nat _.0) -> Bool)))
	((run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons 0 1)) t)) '(Bool))
	((run* (t) (!- '() '((lambda (x) (car x)) (cons (cons 0 0) #f)) t)) '((pairof Nat Nat)))
	((run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons #f 0)) t)) '())
	((run* (t) (!- '() '(lambda (x) (zero? (cdr x))) t)) '(((pairof _.0 Nat) -> Bool)))
	((run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 1)) t)) '(Bool))
	((run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 #f)) t)) '())
	((run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons #f 0)) t)) '(Bool))
	((run* (q)
	   (!- '() '(let ([f (lambda (x) x)])
                       (if (f #t) (f (cons (f 4) 5)) (f (cons 5 (f 6)))))
		q)) '((pairof Nat Nat)))
        ((run* (q)
           (!- '() '(let ([f (lambda (x) #t)])
                      (if #t (f (f 5)) (f #t)))
               q))
         '())))))


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

(test-file #:file-name "a11.rkt")
