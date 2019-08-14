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
              (test-suite "power"
                          (test-equal? "case1"
                                       (run-writer (powerXpartials 2 6))
                                       '((2 4 8) . 64))
                          (test-equal? "case2"
                                       (run-writer (powerXpartials 3 5))
                                       '((3 9 81) . 243))
                          (test-equal? "case3"
                                       (run-writer (powerXpartials 5 7))
                                       '((5 25 125 15625) . 78125)))
              (test-suite "replace-with-count"
                          (test-equal? "case1"
                                       ((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
                                       '(4 . (a 0 (t 1 (e 2 t ((n . m) . 3) . f) . t) . r)))
                          (test-equal? "case2"
                                       ((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
                                       '(3 . (((h (i s . 0) . a) 1 s 2 e . n) . m)))
                          (test-equal? "case3"
                                       ((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)
                                       '(6 . (1 (h (2 s . 3) . 4) . 5))))
              (test-suite "reciprocal"
                          (test-equal? "case1"
                                       (reciprocal 0)
                                       (Nothing))
                          (test-equal? "case2"
                                       (reciprocal 2)
                                       (Just (/ 1 2))))
              ))

(run-tests tests)
