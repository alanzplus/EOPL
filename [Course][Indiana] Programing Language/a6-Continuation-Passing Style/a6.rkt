#lang racket

(provide binary-to-decimal)
(provide binary-to-decimal-cps)
(provide times)
(provide times-cps)
(provide times-cps-shortcut)
(provide plus)
(provide plus-cps)
(provide remv-first-9*)
(provide remv-first-9*-cps)
(provide cons-cell-count)
(provide cons-cell-count-cps)
(provide find)
(provide find-cps)
(provide ack)
(provide ack-cps)
(provide fib)
(provide fib-cps)
(provide unfold)
(provide unfold-cps)
(provide unify)
(provide unify-cps)
(provide M)
(provide M-cps)
(provide use-of-M)
(provide use-of-M-cps)
(provide empty-k)
(provide empty-s)

(define empty-k
  (lambda ()
    (let ([once-only #f])
      (lambda (v)
        (if once-only
          (error 'empty-k "You can only invoke the empty continuation once")
          (begin
            (set! once-only #t)
            v))))))

; 1. binary-to-decimal
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else
        (binary-to-decimal-cps
          (cdr n) 
          (lambda (v)
            (k (+ (car n) (* 2 v)))))])))

; 2. times
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else
        (times-cps (cdr ls)
                   (lambda (v)
                     (k (* (car ls) v))))])))

; 3. times-cps-shortcut
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else
        (times-cps-shortcut
          (cdr ls)
          (lambda (v)
            (k (* (car ls) v))))])))

; 4. plus
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (lambda (m k)
    (lambda (n)
      (k (+ m n)))))

; 5. remv-first-9*
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else
           (cons (remv-first-9* (car ls))
                 (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps
         (car ls)
         (lambda (v1)
           (if (equal? (car ls) v1)
             (remv-first-9*-cps
               (cdr ls)
               (lambda (v2)
                 (k (cons (car ls) v2))))
             (remv-first-9*-cps
               (car ls)
               (lambda (v2)
                 (k (cons v2 (cdr ls))))))))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else
        (remv-first-9*-cps
          (cdr ls)
          (lambda (v)
            (k (cons (car ls) v))))])))

; 6. cons-cell-count
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps
         (car ls)
         (lambda (v1)
           (cons-cell-count-cps
             (cdr ls)
             (lambda (v2)
               (k (add1 (+ v1 v2)))))))]
      [else (k 0)])))

; 7. find
(define find
  (lambda (u s)
    (let ([pr (assv u s)])
      (if pr (find (cdr pr) s) u))))

(define find-cps
  (lambda (u s k)
    (let ([pr (assv u s)])
      (if pr
        (find-cps (cdr pr) s k)
        (k u)))))

; 8 ack
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v)
                                  (ack-cps (sub1 m) v k)))])))
; 9 fib
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
         [(zero? n) 0]
         [(zero? (sub1 n)) 1]
         [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
  (lambda (n k)
    ((lambda (fib)
       (fib fib n k))
     (lambda (fib n k)
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib fib (sub1 n) (lambda (v1)
                                   (fib fib (sub1 (sub1 n)) (lambda (v2)
                                                              (k (+ v1 v2))))))])))))

; 10 unfold
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
         (if (p seed)
           ans
           ((h h) (g seed) (cons (f seed) ans))))))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h)
       ((h h) seed '() k))
     (lambda (h)
       (lambda (seed ans k)
         (if (p seed)
           (k ans)
           ((h h) (g seed) (cons (f seed) ans) (lambda (v) (k v)))))))))

; 11 unify
(define empty-s
  (lambda () '()))

(define unify
  (lambda (u v s)
    (cond
      [(eqv? u v) s]
      [(number? u) (cons (cons u v) s)]
      [(number? v) (unify v u s)]
      [(pair? u)
       (if (pair? v)
         (let ([s (unify (find (car u) s) (find (car v) s) s)])
           (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
         #f)]
      [else #f])))

(define unify-cps
  (lambda (u v s k)
    (cond
      [(eqv? u v) (k s)]
      [(number? u) (k (cons (cons u v) s))]
      [(number? v) (unify-cps v u s k)]
      [(pair? u)
       (if (pair? v)
         (find-cps
           (car u)
           s
           (lambda (v1)
             (find-cps
               (car v)
               s
               (lambda (v2)
                 (unify-cps
                   v1
                   v2
                   s
                   (lambda (v3)
                     (if v3
                       (find-cps
                         (cdr u)
                         v3
                         (lambda (v4)
                           (find-cps
                             (cdr v)
                             v3
                             (lambda (v5)
                               (unify-cps
                                 v4
                                 v5
                                 v3
                                 (lambda (v6)
                                   (k v6)))))))
                       (k #f))))))))
         (k #f))]
       [else (k #f)])))

; 12 M
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        [(null? ls) '()]
        [else (cons (f (car ls)) ((M f) (cdr ls)))]))))

(define M-cps
  (lambda (f)
    (lambda (ls k)
      (cond
        [(null? ls) (k '())]
        [else (f (car ls) (lambda (v1)
                            ((M-cps f) (cdr ls) (lambda (v2)
                                                  (k (cons v1 v2))))))]))))

; 13 use-of-M
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n)))) '(1 2 3 4 5) (empty-k)))
