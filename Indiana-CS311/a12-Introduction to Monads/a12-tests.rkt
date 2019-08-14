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
              (test-suite "partition"
                          (test-equal? "case1"
                                       (run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
                                       '((1 3 5 7 9) . (2 4 6 8 10)))
                          (test-equal? "case2"
                                       (run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))
                                       '((2 4 6 8 10) . (1 3 5 7 9))))
              ))

(run-tests tests)
