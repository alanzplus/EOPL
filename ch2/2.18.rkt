#lang eopl

(provide number->sequence)
(provide current-element)
(provide move-to-left)
(provide move-to-right)
(provide insert-to-left)
(provide insert-to-right)

(define number->sequence
  (lambda (num)
    (list num '() '())))

(define current-element
  (lambda (lst)
    (car lst)))

(define move-to-left
  (lambda (lst)
    (list
      (car (cadr lst))
      (cdr (cadr lst))
      (cons (current-element lst) (car (cdr (cdr lst)))))))

(define move-to-right
  (lambda (lst)
    (list
      (car (car (cdr (cdr lst))))
      (cons
        (current-element lst)
        (car (cdr lst)))
      (cdr (car (cdr (cdr lst)))))))

(define insert-to-left
  (lambda (num lst)
    (list
      (current-element lst)
      (cons
        num
        (car (cdr lst)))
      (car (cdr (cdr lst))))))

(define insert-to-right
  (lambda (num lst)
    (list
      (current-element lst)
      (car (cdr lst))
      (cons
        num
        (car (cdr (cdr lst)))))))
