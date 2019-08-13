#lang racket

(require rackunit "a12.rkt")
(require rackunit "monads.rkt")
(require rackunit/text-ui)


(define tests
  (test-suite "A12:"
              (test-suite "findf-maybe"
                          (test-equal? "case1"
                                       (findf-maybe symbol? '(1 2 c))
                                       (Just 'c))
                          (test-equal? "case2"
                                       (findf-maybe boolean? '(#f 1 2 c))
                                       (Just #f))
                          (test-equal? "case3"
                                       (findf-maybe number? '(a b c))
                                       (Nothing)))
              ))

(run-tests tests)
