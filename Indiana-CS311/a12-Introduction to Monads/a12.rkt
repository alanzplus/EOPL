#lang racket

(require "monads.rkt")
(provide findf-maybe)
(provide partition-writer)
(provide powerXpartials)
(provide replace-with-count)
(provide reciprocal)
(provide traverse-reciprocal)

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

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (bind-writer
         (powerXpartials x (sub1 n))
         (lambda (a)
           (Writer (list a) (* a x))))]
      [(even? n)
       (bind-writer
         (powerXpartials x (/ n 2))
         (lambda (a)
           (Writer (list a) (* a a))))])))

(define replace-with-count
  (lambda (s tr)
    (cond
      [(pair? tr)
       (bind-state
         (replace-with-count s (car tr))
         (lambda (a)
           (bind-state
             (replace-with-count s (cdr tr))
             (lambda (b)
               (inj-state (cons a b))))))]
      [(eq? s tr)
       (bind-state
         (get)
         (lambda (s)
           (bind-state
             (put (add1 s))
             (lambda (_)
               (inj-state s)))))]
      [else (inj-state tr)])))

(define reciprocal
  (lambda (n)
    (if (zero? n) (Nothing) (Just (/ 1 n)))))

(define traverse
  (lambda (inj bind f)
    (letrec
      ([trav
         (lambda (tree)
           (cond
             [(pair? tree)
              (go-on ([a (trav (car tree))]
                      [b (trav (cdr tree))])
                     (inj (cons a b)))]
             [else (f tree)]))])
      trav)))

(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))
