#lang racket

(define empty-k
  (lambda ()
    '(empty-k)))

(define ack-k
  (lambda (m k)
    `(ack-k ,m ,k)))

(define ack-apply-k
  (lambda (k v)
    (match k
           ['(empty-k) v]
           [`(ack-k ,m ,k) (ack (sub1 m) v k)])))

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (ack-apply-k k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else
        (ack m (sub1 n) (ack-k m k))])))

(ack 2 2 (empty-k))
