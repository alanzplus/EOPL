#lang racket
(provide ack-reg-driver)
(provide depth-reg-driver)
(provide fact-reg-driver)
(provide pascal-reg-driver)
(provide fib-cps)

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
  (letrec ([n* 'uninit]
           [k* 'uninit]
           [v* 'uninit]
           [empty-k (lambda () '(empty-k))]
           [cont (lambda (k n) `(cont ,k ,n))]
           [apply-k (lambda ()
                      (match k*
                             ['(empty-k) v*]
                             [`(cont ,saved-k ,n)
                               (begin
                                 (set! k* saved-k)
                                 (set! v* (* n v*))
                                 (apply-k))]))]
           [fact (lambda ()
                   (cond
                     [(zero? n*)
                      (begin
                        (set! v* 1)
                        (apply-k))]
                     [else
                       (begin
                         (set! k* (cont k* n*))
                         (set! n* (sub1 n*))
                         (fact))]))])
    (lambda (n)
      (begin
        (set! n* n)
        (set! k* (empty-k))
        (fact)))))

(define pascal-reg-driver
  (letrec ([n* 'uninit]
           [k* 'uninit]
           [v* 'uninit]
           [m* 'uninit]
           [a* 'uninit]
           [empty-k (lambda () '(empty-k))]
           [init-k (lambda (saved-k) `(init-k ,saved-k))]
           [big-k (lambda (a m saved-k) `(big-k ,a ,m ,saved-k))]
           [small-k (lambda (a saved-k) `(small-k ,a ,saved-k))]
           [apply-k (lambda ()
                      (match k*
                             ['(empty-k) v*]
                             [`(init-k ,saved-k)
                               (begin
                                 (set! m* 1)
                                 (set! a* 0)
                                 (set! k* saved-k)
                                 (v*))]
                             [`(big-k ,a ,m ,saved-k)
                               (begin
                                 (set! m* (add1 m))
                                 (set! a* a)
                                 (set! k* (small-k a saved-k))
                                 (v*))]
                             [`(small-k ,a ,saved-k)
                               (begin
                                 (set! k* saved-k)
                                 (set! v* (cons a v*))
                                 (apply-k))]))]
           [f (lambda ()
                (cond
                  [(> m* n*)
                   (begin
                     (set! v* '())
                     (apply-k))]
                  [else (let ([a (+ a* m*)])
                          (begin
                            (set! k* (big-k a m* k*))
                            (apply-k)))]))])
    (lambda (n)
      (begin
        (set! n* n)
        (set! k* (init-k (empty-k)))
        (set! v* f)
        (apply-k)))))

(define fib-cps
  (letrec ([n* 'uninit]
           [k* 'uninit]
           [v* 'uninit]
           [empty-k (lambda () '(empty-k))]
           [big-k (lambda (n saved-k) `(big-k ,n ,saved-k))]
           [small-k (lambda (v saved-k) `(small-k ,v ,saved-k))]
           [apply-k (lambda ()
                      (match k*
                             [`(big-k ,n ,saved-k)
                               (begin
                                 (set! n* (sub1 n))
                                 (set! k* (small-k v* saved-k))
                                 (fib))]
                             [`(small-k ,v1 ,saved-k) 
                               (begin
                                 (set! k* saved-k)
                                 (set! v* (+ v1 v*))
                                 (apply-k))]
                             [`(empty-k) v*]))]
           [fib (lambda ()
                  (cond
                    [(and (not (negative? n*)) (< n* 2))
                     (begin
                       (set! v* n*)
                       (apply-k))]
                    [else
                      (begin
                        (set! k* (big-k (sub1 n*) k*))
                        (set! n* (sub1 n*))
                        (fib))]))])
    (lambda (n)
      (begin
        (set! n* n)
        (set! k* (empty-k))
        (fib)))))
