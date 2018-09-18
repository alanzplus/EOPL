#lang eopl

(provide bintree)
(provide leaf-node)
(provide interior-node)
(provide bintree-to-list)

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (val) (list 'leaf-node val))
      (interior-node
        (val l-tree r-tree)
        (list
          'interior-node
          val
          (bintree-to-list l-tree)
          (bintree-to-list r-tree))))))
