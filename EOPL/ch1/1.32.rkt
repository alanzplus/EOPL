#lang eopl

(require "1.31.rkt")

(define double-tree
  (lambda (btree)
    (if (null? btree)
        '()
        (if (leaf? btree)
            (leaf (* 2 (contents-of btree)))
            (interior-node
              (contents-of btree)
              (double-tree (lson btree))
              (double-tree (rson btree)))))))
    
(provide double-tree)
