#lang racket
(provide ack-reg-driver)
(provide depth-reg-driver)
(provide fact-reg-driver)

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
  (letrec ([ls* 'uninit]
           [k* 'uninit]
           [v* 'unint]
           [empty-k (lambda () '(empty-k))]
           [big-k (lambda (ls k) `(big-k ,ls ,k))]
           [small-k (lambda (l k) `(small-k ,l ,k))]
           [apply-k (lambda ()
                      (match k*
                             ['(empty-k) v*]
                             [`(big-k ,ls ,k)
                               (begin
                                 (set! ls* (cdr ls))
                                 (set! k* (small-k v* k))
                                 (depth))]
                             [`(small-k ,l ,k)
                               (let ([l (add1 l)])
                                 (if (< l v*)
                                     (begin
                                       (set! k* k)
                                       (apply-k))
                                     (begin
                                       (set! k* k)
                                       (set! v* l)
                                       (apply-k))))]))]
           [depth (lambda ()
                    (cond
                      [(null? ls*)
                       (begin
                         (set! v* 1)
                         (apply-k))]
                      [(pair? (car ls*))
                       (begin
                         (set! k* (big-k ls* k*))
                         (set! ls* (car ls*))
                         (depth))]
                      [else 
                        (begin
                          (set! ls* (cdr ls*))
                          (depth))]))])
    (lambda (ls)
      (begin
        (set! ls* ls)
        (set! k* (empty-k))
        (depth)))))

(define fact-reg-driver
  (letrec ([empty-k (lambda () '(empty-k))]
           [cont (lambda (k n) `(cont ,k ,n))]
           [apply-k (lambda (k v)
                      (match k
                             ['(empty-k) v]
                             [`(cont ,saved-k ,n)
                               (apply-k saved-k (* n v))]))]
           [fact (lambda (n k)
                   ((lambda (fact k)
                      (fact fact n k))
                    (lambda (fact n k)
                      (cond
                        [(zero? n) (apply-k k 1)]
                        [else (fact fact (sub1 n) (cont k n))]))
                    k))])
    (lambda (n)
      (fact n (empty-k)))))
