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
  (lambda (#:file-name (file "./a5.rkt")
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
  (test-suite "a5"
    (test-suite "cbr"
      (test-equal-if-defined val-of-cbr          
        ;; Making sure set! works
        ((val-of-cbr
      '((lambda (x) (begin2 (set! x #t)
                (if x 3 5))) #f)
      (empty-env))
     3)
    
    ;; Returns 4 under CBR...
    ((val-of-cbr
      '((lambda (a)
          ((lambda (p)
         (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
      (empty-env))
     4)
    
    ;; returns 44 under CBR...
    ((val-of-cbr
      '((lambda (f)
          ((lambda (g)
         ((lambda (z) (begin2
                   (g z)
                   z))
          55))
           (lambda (y) (f y)))) (lambda (x) (set! x 44)))
      (empty-env))
     44)

    ;; Returns 44 under CBR...
    ((val-of-cbr
      '((lambda (swap)
          ((lambda (a)
         ((lambda (b)
            (begin2
             ((swap a) b)
             a)) 44)) 33))
        (lambda (x)
          (lambda (y)
        ((lambda (temp)
           (begin2
            (set! x y)
            (set! y temp))) x))))
      (empty-env))
     44)
    ))
    (test-suite "cbv"
      (test-equal-if-defined val-of-cbv
        ;; ...but returns 3 under CBV.
        ((val-of-cbv
      '((lambda (a)
          ((lambda (p)
         (begin2
          (p a)
          a)) (lambda (x) (set! x 4)))) 3)
      (empty-env))
     3)
    
    ;; ...but returns 55 under CBV!  You can change the "begin2" to
    ;; "begin" and evaluate this in the Scheme REPL as evidence that
    ;; Scheme uses CBV.
    ((val-of-cbv
      '((lambda (f)
          ((lambda (g)
         ((lambda (z) (begin2
                   (g z)
                   z))
          55))
           (lambda (y) (f y)))) (lambda (x) (set! x 44)))
      (empty-env))
     55)
    
    ;; ...but returns 33 under CBV.
    ((val-of-cbv
      '((lambda (swap)
          ((lambda (a)
         ((lambda (b)
            (begin2
             ((swap a) b)
             a)) 44)) 33))
        (lambda (x)
          (lambda (y)
        ((lambda (temp)
           (begin2
            (set! x y)
            (set! y temp))) x))))
      (empty-env))
     33)))
    
    (test-suite "call-by-name"
      (test-equal-if-defined val-of-cbname
    ;;P(false positive) <= .01                                            
    ((let ((random-sieve 
        '((lambda (n)
            (if (zero? n)
            (if (zero? n) 
                (if (zero? n) 
                (if (zero? n) 
                    (if (zero? n) 
                    (if (zero? n) 
                        (if (zero? n) #t 
                        #f) 
                        #f) 
                    #f) 
                    #f) 
                #f) 
                #f)
            (if (zero? n) #f 
                (if (zero? n) #f 
                (if (zero? n) #f 
                    (if (zero? n) #f 
                    (if (zero? n) #f 
                        (if (zero? n) #f 
                        #t))))))))
          (random 2))))
       (val-of-cbname random-sieve (empty-env)))
     #f)

    ;; Does not terminate with val-of-cbr or val-of-cbv -- try it!
    ((val-of-cbname
      '((lambda (z) 100)
        ((lambda (x) (x x)) (lambda (x) (x x))))
      (empty-env))
     100)))

    (test-suite "cbneed"
      (test-equal-if-defined val-of-cbneed
        ;; call-by-need                 
        ((let ((random-sieve 
        '((lambda (n)
            (if (zero? n)
            (if (zero? n) 
                (if (zero? n) 
                (if (zero? n) 
                    (if (zero? n) 
                    (if (zero? n) 
                        (if (zero? n) #t 
                        #f) 
                        #f) 
                    #f) 
                    #f) 
                #f) 
                #f)
            (if (zero? n) #f 
                (if (zero? n) #f 
                (if (zero? n) #f 
                    (if (zero? n) #f 
                    (if (zero? n) #f 
                        (if (zero? n) #f 
                        #t))))))))
          (random 2))))
       (val-of-cbneed random-sieve (empty-env)))
     #t)))

      (test-suite "cons-should-not-evaluate"
    (test-equal-if-defined val-of-cbv
      ((let ((cons-test
          '(let ((fix 
              (lambda (f)
                ((lambda (x) (f (lambda (v) ((x x) v))))
                 (lambda (x) (f (lambda (v) ((x x) v))))))))
             (let ((map 
                (fix 
                 (lambda (map)
                   (lambda (f)
                 (lambda (l)
                   (if (null? l)
                       '()
                       (cons^ (f (car^ l))
                          ((map f) (cdr^ l))))))))))
               (let ((take 
                  (fix 
                   (lambda (take)
                 (lambda (l)
                   (lambda (n)
                     (if (zero? n)
                     '()
                     (cons (car^ l) 
                           ((take (cdr^ l)) (sub1 n))))))))))
             ((take 
               ((fix 
                 (lambda (m)
                   (lambda (i)
                 (cons^ 1 
                    ((map (lambda (x) 
                        (add1 x))) 
                     (m i)))))) 
                0)) 
              5))))))
         (val-of-cbv cons-test (empty-env)))
       '(1 2 3 4 5))))))


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
