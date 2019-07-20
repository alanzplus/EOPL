#lang racket

(require rackunit "a8.rkt")
(require rackunit/text-ui)

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
              (lambda (l)
                (depth (cdr ls)
                       (lambda (r)
                         (let ((l (add1 l)))
                           (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define tests
  (test-suite "A8:"
              (test-suite "ack"
                          (test-equal? "case1"
                                       (ack-reg-driver 2 2) 7)
                          (test-equal? "case2"
                                       (ack-reg-driver 1 1) (ack 1 1 (lambda (v) v)))
                          (test-equal? "case3"
                                       (ack-reg-driver 3 3) (ack 3 3 (lambda (v) v)))
                          )
              (test-suite "depth"
                          (test-equal? "case1"
                                       (depth-reg-driver '(1 (2 (3 (4))))) (depth '(1 (2 (3 (4)))) (lambda (v) v)))
                          (test-equal? "case2"
                                       (depth-reg-driver '(1 2 2 (2 (3 (4))))) (depth '(1 2 2 (2 (3 (4)))) (lambda (v) v))))
              )
  )

(run-tests tests)
