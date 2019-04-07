
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
    -1)))

(run-tests common-test)