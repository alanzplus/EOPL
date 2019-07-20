#lang racket

(require rackunit "a8.rkt")
(require rackunit/text-ui)

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define tests
  (test-suite "A8:"
              (test-suite ""
                          (test-equal? "case1"
                                      (ack-reg-driver 2 2) 7)
                          (test-equal? "case2"
                                       (ack-reg-driver 1 1) (ack 1 1 (lambda (v) v)))
                          (test-equal? "case3"
                                       (ack-reg-driver 3 3) (ack 3 3 (lambda (v) v)))
                          )
              )
)

(run-tests tests)
