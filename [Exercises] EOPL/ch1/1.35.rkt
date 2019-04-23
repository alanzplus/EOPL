#lang eopl

(require "bintree.rkt")

(define number-leaves
  (lambda (tree)
    (aux 0 tree)))

(define aux
  (lambda (num tree)
    (cond
      ((null? tree) '())
      ((leaf? tree) (leaf num))
      (else
        (interior-node
          (contents-of tree)
          (aux num (lson tree))
          (aux (+ num (count (lson tree))) (rson tree)))))))

(define count
  (lambda (tree)
    (cond
      ((null? tree) 0)
      ((leaf? tree) 1)
      (else
        (+
          (count (lson tree))
          (count (rson tree)))))))

(provide number-leaves)
