#lang eopl

; least-significant bigit first

(define BASE 16)

(define zero
  (lambda () '()))

(define is-zero?
  (lambda (num)
    (null? num)))

(define successor
  (lambda (num)
    (remove-leading-zero (list-add 1 num))))

(define predecessor
  (lambda (num)
    (remove-leading-zero (list-add -1 num))))

(define list-add
  (lambda (inc lst)
    (if (null? lst)
        (list inc)
        (let ((sum (+ inc (car lst))))
          (let ((carry (floor (/ sum BASE)))
                (rem (modulo sum BASE)))
               (cons
                rem
                (if (= 0 carry)
                    (cdr lst)
                    (list-add carry (cdr lst)))))))))

; remove zero in most-significant bit
(define remove-leading-zero
  (lambda (lst)
    (if (null? lst)
        '()
        (if (null? (cdr lst))
            (if (= (car lst) 0)
                '()
                lst)
            (cons (car lst) (remove-leading-zero (cdr lst)))))))

(provide zero)
(provide is-zero?)
(provide successor)
(provide predecessor)
