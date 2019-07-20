#lang racket
(provide ack-reg-driver)
(provide depth-reg-driver)

(define ack-reg-driver
  (letrec ([m* 'uninit]
           [n* 'uninit]
           [k* 'uninit]
           [v* 'uninit]
           [empty-k (lambda () '(empty-k))]
           [ack-k (lambda (m k) `(ack-k ,m ,k))]
           [ack (lambda ()
                  (cond
                    [(zero? m*)
                     (begin
                       (set! v* (add1 n*))
                       (apply-k))]
                    [(zero? n*)
                     (begin
                       (set! m* (sub1 m*))
                       (set! n* 1)
                       (ack))]
                    [else
                      (begin
                        (set! k* (ack-k m* k*))
                        (set! n* (sub1 n*))
                        (ack))]))]
           [apply-k (lambda ()
                      (match k*
                             ['(empty-k) v*]
                             [`(ack-k ,m ,k)
                               (begin
                                 (set! m* (sub1 m))
                                 (set! n* v*)
                                 (set! k* k)
                                 (ack))]))])
    (lambda (m n)
      (begin
        (set! m* m)
        (set! n* n)
        (set! k* (empty-k))
        (ack)))))

(define depth-reg-driver
  (letrec ([empty-k (lambda () '(empty-k))]
           [big-k (lambda (ls k) `(big-k ,ls ,k))]
           [small-k (lambda (l k) `(small-k ,l ,k))]
           [apply-k (lambda (k v)
                      (match k
                             ['(empty-k) v]
                             [`(big-k ,ls ,k)
                               (depth (cdr ls) (small-k v k))]
                             [`(small-k ,l ,k)
                               (let ([l (add1 l)])
                                 (if (< l v) (apply-k k v) (apply-k k l)))]))]
           [depth (lambda (ls k)
                    (cond
                      [(null? ls) (apply-k k 1)]
                      [(pair? (car ls)) (depth (car ls) (big-k ls k))]
                      [else (depth (cdr ls) k)]))])
    (lambda (ls)
      (depth ls (empty-k)))))
