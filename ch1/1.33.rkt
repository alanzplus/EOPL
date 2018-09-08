#lang eopl

(require "1.31.rkt")

(define mark-leaves-with-red-depth
  (lambda (tree)
    (auxi tree 0)))

(define auxi
  (lambda (tree red-depth)
    (cond
      ((null? tree) '())
      ((leaf? tree) (leaf red-depth))
      ((eqv? (contents-of tree) 'red)
        (interior-node
          (contents-of tree)
          (auxi (lson tree) (+ 1 red-depth))
          (auxi (rson tree) (+ 1 red-depth))))
      (else
        (interior-node
          (contents-of tree)
          (auxi (lson tree) red-depth)
          (auxi (rson tree) red-depth))))))

(provide mark-leaves-with-red-depth)
