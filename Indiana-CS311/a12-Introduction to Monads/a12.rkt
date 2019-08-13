#lang racket

(require "monads.rkt")
(provide findf-maybe)

(define findf-maybe
  (lambda (predicate? ls)
    (cond
      [(null? ls) (Nothing)]
      [(predicate? (car ls))
       (Just (car ls))]
      [else
        (findf-maybe predicate? (cdr ls))])))
