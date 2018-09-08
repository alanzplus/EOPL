#lang eopl

(require "bst.rkt")

(define path
  (lambda (int tree)
    (aux int tree '())))

(define aux
  (lambda (int tree paths)
    (if (null? tree)
        '()
        (let ((val (val-of-node tree)))
          (cond
            ((> int val)
             (aux int (rtree tree) (append paths (list 'right))))
            ((< int val)
             (aux int (ltree tree) (append paths (list 'left))))
            (else paths))))))

(provide path)
