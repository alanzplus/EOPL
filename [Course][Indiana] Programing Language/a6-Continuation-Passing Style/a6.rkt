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
(provide empty-k)

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
