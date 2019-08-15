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
              (test-suite "traverse-reciprocal"
                          (test-equal? "case1"
                                       (traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
                                       (Just '((1 . 1/2) . (1/3 . (1/4 . 1/5)))))
                          (test-equal? "case2"
                                       (traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
                                       (Nothing)))
              (test-suite "halve"
                          (test-equal? "case1"
                                       (run-writer (halve 6))
                                       '(() . 3))
                          (test-equal? "case2"
                                       (run-writer (halve 5))
                                       '((5) . 5)))
              (test-suite "traverse-halve"
                          (test-equal? "case1"
                                       (run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))
                                       '((1 3 5) . ((1 . 1) . (3 . (2 . 5))))))
              (test-suite "state/sum"
                          (test-equal? "case1"
                                       ((run-state (state/sum 5)) 0)
                                       '(5 . 0))
                          (test-equal? "case2"
                                       ((run-state (state/sum 2)) 0)
                                       '(2 . 0))
                          (test-equal? "cas3"
                                       ((run-state (state/sum 2)) 3)
                                       '(5 . 3)))
              (test-suite "traverse-state/sum"
                          ((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)
                          '(15 . ((0 . 1) 3 6 . 10)))
              (test-suite "value-of-cps"
                          (test-equal? "case1"
                                       ((run-cont (value-of-cps '1 (empty-env))) (lambda (v) v)) 1)
                          (test-equal? "case2"
                                       ((run-cont (value-of-cps '#f (empty-env))) (lambda (v) v)) #f)
                          (test-equal? "case3"
                                       ((run-cont (value-of-cps 'a (extend-env 'a 10 (empty-env)))) (lambda (v) v)) 10)
                          (test-equal? "case4"
                                       ((run-cont (value-of-cps '(* a 20) (extend-env 'a 10 (empty-env)))) (lambda (v) v)) 200)
                          (test-equal? "case5"
                                       ((run-cont (value-of-cps '(sub1 a) (extend-env 'a 10 (empty-env)))) (lambda (v) v)) 9)
                          (test-equal? "case6"
                                       ((run-cont (value-of-cps '(zero? (sub1 a)) (extend-env 'a 1 (empty-env)))) (lambda (v) v)) #t)
                          (test-equal? "case7"
                                       ((run-cont (value-of-cps '(if (zero? 0) (sub1 20) (sub1 10)) (empty-env))) (lambda (v) v)) 19)
                          (test-equal? "case8"
                                       ((run-cont (value-of-cps '((lambda (x) (* x 3)) 10) (empty-env))) (lambda (v) v)) 30)
                          (test-equal? "case9"
                                       ((run-cont (value-of-cps
                                                    '((lambda (f)
                                                        ((f f) 5))
                                                      (lambda (f)
                                                        (lambda (n)
                                                          (if (zero? n)
                                                            1
                                                            (* n ((f f) (sub1 n)))))))
                                                    (empty-env))) (lambda (v) v)) 120)
                          (test-equal? "case10"
                                       ((run-cont (value-of-cps
                                                    '(* 3 (capture q (* 2 (return q 4))))
                                                    (empty-env))) (lambda (v) v)) 12)

                          )
              ))

(run-tests tests)
