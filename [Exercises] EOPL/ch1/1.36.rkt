#lang eopl

(define number-elements
  (lambda (lst)
    (if (null? lst)
      '()
      (g (list 0 (car lst))
         (number-elements (cdr lst))))))

(define g
  (lambda (lst1 lst2)
    (cons
      lst1
      (aux lst2))))

(define aux
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((fst (car lst)))
          (cons
            (list (+ 1 (car fst)) (cadr fst))
            (aux (cdr lst)))))))

(provide number-elements)
