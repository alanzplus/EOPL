#lang eopl

(define interior-node
  (lambda (content ltree rtree)
    (list content ltree rtree)))

(define leaf
  (lambda (content)
    (list content)))

(define leaf?
  (lambda (node)
    (and (not (null? node)) (number? (car node)))))

(define lson
  (lambda (node)
    (cadr node)))

(define rson
  (lambda (node)
    (cadr (cdr node))))

(define contents-of
  (lambda (node)
    (if (null? node) '() (car node))))

(define interior-node?
  (lambda (node)
    (if (and (not (null? node)) (not (leaf? node))) #t #f)))

(define tree-equal?
  (lambda (tree1 tree2)
    (let ((tree1-is-leaf? (leaf? tree1))
          (tree2-is-leaf? (leaf? tree2))
          (val1 (contents-of tree1))
          (val2 (contents-of tree2)))
        (cond
          ((and tree1-is-leaf? tree2-is-leaf?)
            (if (eqv? val1 val2) #t #f))
          ((and
            (interior-node? tree1)
            (interior-node? tree2))
           (if (eqv? val1 val2)
            (and
              (tree-equal? (lson tree1) (lson tree2))
              (tree-equal? (rson tree1) (rson tree2)))
            #f))
          (else
            (if (and (null? tree1) (null? tree2)) #t #f))))))

(provide tree-equal?)
(provide interior-node?)
(provide interior-node)
(provide leaf)
(provide leaf?)
(provide lson)
(provide rson)
(provide contents-of)
