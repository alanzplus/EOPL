#lang eopl

(provide number->bintree)
(provide insert-to-left)
(provide insert-to-right)
(provide at-leaf?)
(provide current-elemnt)

(define at-leaf?
  (lambda (tree)
    (and (null? (car (cdr tree)))
         (null? (car (cdr (cdr tree)))))))

(define number->bintree
  (lambda (num)
    (list num '() '())))

(define insert-to-left
  (lambda (num tree)
    (if (at-leaf? tree)
        (list
          (car tree)
          (list num)
          '())
        (list
          (car tree)
          (list
            num
            (car (cdr tree))
            '())
          (car (cdr (cdr tree)))))))

(define insert-to-right
  (lambda (num tree)
    (if (at-leaf? tree)
        (list
          (car tree)
          '()
          (list num))
        (list
          (car tree)
          (car (cdr tree))
          (list
            num
            (car (cdr (cdr tree)))
            '())))))

(define current-element
  (lambda (tree)
    (car tree)))
