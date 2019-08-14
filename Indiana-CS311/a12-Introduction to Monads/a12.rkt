#lang racket

(require "monads.rkt")
(provide findf-maybe)
(provide partition-writer)

(define findf-maybe
  (lambda (predicate? ls)
    (cond
      [(null? ls) (Nothing)]
      [(predicate? (car ls))
       (Just (car ls))]
      [else
        (findf-maybe predicate? (cdr ls))])))

(define partition-writer
  (lambda (predicate? ls)
    (cond
      [(null? ls) (inj-writer '())]
      [(predicate? (car ls))
       (bind-writer
         (partition-writer predicate? (cdr ls))
         (lambda (a)
           (inj-writer (cons (car ls) a))))]
      [else
        (bind-writer
          (tell (car ls))
          (lambda (_)
            (partition-writer predicate? (cdr ls))))])))
