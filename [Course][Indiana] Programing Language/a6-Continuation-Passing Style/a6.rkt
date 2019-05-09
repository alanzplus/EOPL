#lang racket

(provide binary-to-decimal)
(provide binary-to-decimal-cps)
(provide times)
(provide times-cps)
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
