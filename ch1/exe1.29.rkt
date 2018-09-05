#lang eopl

; merge function
(require "exe1.28.rkt")

(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (if (= (length loi) 1)
            loi
            (let ((split-lists (split loi)))
              (merge (sort (car split-lists)) (sort (cadr split-lists))))))))

(define split
  (lambda (loi)
    (if (= (length loi) 1)
      (list loi '())
      (let ((len (length loi))) 
           (list
            (collect-n loi 0 0 (- (floor (/ len 2)) 1))
            (collect-n loi 0 (floor (/ len 2)) (- len 1)))))))

(define collect-n
  (lambda (loi idx beg end)
    (cond
      ((< idx beg) (collect-n (cdr loi) (+ idx 1) beg end))
      ((and (<= beg idx) (<= idx end))
       (cons (car loi) (collect-n (cdr loi) (+ idx 1) beg end)))
      (else '()))))

(provide sort)
