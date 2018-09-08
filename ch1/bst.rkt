#lang eopl

; constructs for binary search tree

(define bst
  (lambda (val left-bst right-bst)
    (list val left-bst right-bst)))

(define null-node?
  (lambda (node)
    (null? node)))

(define val-of-node
  (lambda (node)
    (if (null-node? node) '() (car node))))

(define ltree
  (lambda (node)
    (cadr node)))

(define rtree
  (lambda (node)
    (caddr node)))

(provide ltree)
(provide rtree)
(provide bst)
(provide null-node?)
(provide val-of-node)
