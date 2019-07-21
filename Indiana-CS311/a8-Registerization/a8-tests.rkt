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

(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))

(define pascal
  (lambda (n k)
    (let ((pascal
            (lambda (pascal k)
              (k (lambda (m a k)
                   (cond
                     [(> m n) (k '())]
                     [else (let ((a (+ a m)))
                             (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))

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
              (test-suite "fact"
                          (test-equal? "case1"
                                       (fact-reg-driver 5) (fact 5 (lambda (v) v)))
                          (test-equal? "case2"
                                       (fact-reg-driver 1) (fact 1 (lambda (v) v)))
                          (test-equal? "case3"
                                       (fact-reg-driver 2) (fact 2 (lambda (v) v)))
                          (test-equal? "case4"
                                       (fact-reg-driver 3) (fact 3 (lambda (v) v))))
              (test-suite "pascal"
                          (test-equal? "case1"
                                       (pascal-reg-driver 1) (pascal 1 (lambda (v) v)))
                          (test-equal? "case2"
                                       (pascal-reg-driver 2) (pascal 2 (lambda (v) v)))
                          (test-equal? "case3"
                                       (pascal-reg-driver 3) (pascal 3 (lambda (v) v)))
                          (test-equal? "case4"
                                       (pascal-reg-driver 5) (pascal 5 (lambda (v) v)))
                          (test-equal? "case5"
                                       (pascal-reg-driver 10) (pascal 10 (lambda (v) v))))
              )
  )

(run-tests tests)
