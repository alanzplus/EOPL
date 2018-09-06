#lang eopl

(define sort
  (lambda (pred loi)
    (if (null? loi)
        '()
        (if (= (length loi) 1)
            loi
            (let ((split-lists (split loi)))
              (merge pred (sort pred (car split-lists)) (sort pred (cadr split-lists))))))))

(define merge
  (lambda (pred loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((pred (car loi1) (car loi2)) (cons (car loi1) (merge pred (cdr loi1) loi2)))
      (else (cons (car loi2) (merge pred loi1 (cdr loi2)))))))

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

(provide sort merge)
