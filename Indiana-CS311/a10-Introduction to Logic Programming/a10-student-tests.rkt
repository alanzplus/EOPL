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
  (lambda (#:file-name (file "./a10.rkt")
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
  (test-suite "a10"
    (test-suite "stuttero"
      (test-equal-if-defined stuttero
        ((run 1 (q) (stuttero q '(1 1 2 2 3 3))) '((1 2 3)))
        ((run* (q) (stuttero q '(1 1 2 2 3 3))) '((1 2 3)))
        ((run 1 (q) (fresh (a b c d) 
		      (== q `(,a ,b ,c ,d)) 
		      (stuttero a `(1 ,b ,c 2 3 ,d)))) 
	 '(((1 2 3) 1 2 3)))
        ((run 1 (q) (fresh (a b c d) 
		      (== q `(,a ,b ,c ,d)) 
		      (stuttero `(,b 1) `(,c . ,d)))) 
	 '((_.0 _.1 _.1 (_.1 1 1))))
        ((run 1 (q) (fresh (e f g) 
		      (== q `(,e ,f ,g)) 
		      (stuttero `(,e . ,f) g))) 
	 '((_.0 () (_.0 _.0))))
        ((run 2 (q) (fresh (e f g) 
		      (== q `(,e ,f ,g)) 
		      (stuttero `(,e . ,f) g))) 
	 '((_.0 () (_.0 _.0)) (_.0 (_.1) (_.0 _.0 _.1 _.1))))))
   
    (test-suite "assoco"
      (test-equal-if-defined assoco
        ((run* (q) (assoco 'x '() q)) '())
        ((run* (q) (assoco 'x '((x . 5)) q)) '((x . 5)))
        ((run* (q) (assoco 'x '((y . 6) (x . 5)) q)) '((x . 5)))
        ((run* (q) (assoco 'x '((x . 6) (x . 5)) q)) '((x . 6)))
        ((run* (q) (assoco 'x '((x . 5)) '(x . 5))) '(_.0))
        ((run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 6))) '(_.0))
        ((run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 5))) '())
        ((run* (q) (assoco q '((x . 6) (x . 5)) '(x . 5))) '())
        ((run* (q) (assoco 'x '((x . 6) . ,q) '(x . 6))) '(_.0))
        ((run 4 (q) (assoco 'x q '(x . 5)))
         '(((x . 5) . _.0)
           (((_.0 . _.1) (x . 5) . _.2) (=/= ((_.0 x))))
           (((_.0 . _.1) (_.2 . _.3) (x . 5) . _.4) (=/= ((_.0 x)) ((_.2 x))))
           (((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (x . 5) . _.6)
            (=/= ((_.0 x)) ((_.2 x)) ((_.4 x))))))))

    (test-suite "reverseo"
      (test-equal-if-defined reverseo
        ((run* (q) (reverseo '() q)) '(()))
        ((run* (q) (reverseo '(a) q)) '((a)))
        ((run* (q) (reverseo '(a b c d) q)) '((d c b a)))
        ((run* (q) (fresh (x) (reverseo `(a b ,x c d) q))) '((d c _.0 b a)))
        ((run* (x) (reverseo `(a b ,x d) '(d c b a))) '(c))
        ((run* (x) (reverseo `(a b c d) `(d . ,x))) '((c b a)))
        ((run* (q) (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x))))) '(c))
        ((run 10 (q) (fresh (x y) (reverseo x y) (== `(,x ,y) q))) 
	 '((() ()) ((_.0) (_.0)) ((_.0 _.1) (_.1 _.0))
	   ((_.0 _.1 _.2) (_.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3) (_.3 _.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3 _.4) (_.4 _.3 _.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3 _.4 _.5) (_.5 _.4 _.3 _.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6)
	    (_.6 _.5 _.4 _.3 _.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7)
	    (_.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))
	   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8)
	    (_.8 _.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))))))

    (test-suite "lengtho"
      (test-equal-if-defined lengtho
        ((run 1 (q) (lengtho '() q)) '(()))
        ((run 1 (q) (lengtho '(a b) q)) '((0 1)))
        ((run 1 (q) (lengtho '(a b c) q)) '((1 1)))
        ((run 1 (q) (lengtho '(a b c d e f g) q)) '((1 1 1)))
        ((run 1 (q) (lengtho q (build-num 0))) '(()))
        ((run 1 (q) (lengtho q (build-num 5))) '((_.0 _.1 _.2 _.3 _.4)))
        ((run 10 (q) (fresh (x y) (lengtho x y) (== `(,x ,y) q)))
         '((() ()) ((_.0) (1)) ((_.0 _.1) (0 1)) ((_.0 _.1 _.2) (1 1))
           ((_.0 _.1 _.2 _.3) (0 0 1)) ((_.0 _.1 _.2 _.3 _.4) (1 0 1))
           ((_.0 _.1 _.2 _.3 _.4 _.5) (0 1 1))
           ((_.0 _.1 _.2 _.3 _.4 _.5 _.6) (1 1 1))
           ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7) (0 0 0 1))
           ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8) (1 0 0 1))))))))
    
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

(test-file #:file-name "a10.rkt")
