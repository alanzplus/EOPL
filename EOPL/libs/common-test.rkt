
#lang eopl

(require rackunit "common.rkt")
(require rackunit/text-ui)

(define common-test
  (test-suite
   "list utilities test"
   (check-equal?
    (index-of '(a b c) 'a)
    0)
   (check-equal?
    (index-of '(a b c) 'b)
    1)
   (check-equal?
    (index-of '(a b c) 'c)
    2)
   (check-equal?
    (index-of '(a b c) 'd)
    -1)
   (check-equal?
     (list-index (lambda (x) (eqv? x 'z)) '(a b c))
     #f)
   (check-equal?
     (list-last-index (lambda (x) (eqv? x 'a)) '(a b c a a d z))
     4)
   (check-equal?
     (last '(a))
     'a)
   (check-equal?
     (last '(a b c))
     'c)
   (check-equal?
     (remove-last '(a))
     (list))
   (check-equal?
     (remove-last '(a b))
     '(a))
   (check-equal?
     (remove-last '(a b c))
     '(a b))
   ))

(run-tests common-test)
