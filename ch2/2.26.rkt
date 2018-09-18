#lang eopl

; red-blue-tree ::= red-blue-subtree
; red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                  ::= (blue-node {Red-blue-subtree}*)
;                  ::= (leaf-node Int)

(provide red-node)
(provide blue-node)
(provide leaf-node)
(provide mark-leaves-with-red-depth)

(define-datatype red-blue-tree red-blue-tree?
  (red-node
    (left red-blue-tree?)
    (right red-blue-tree?))
  (blue-node
    (children (list-of red-blue-tree?)))
  (leaf-node
    (int integer?)))

(define list-of
  (lambda (pred)
    (lambda (a-list)
      (or (null? a-list)
          (and (pair? a-list)
               (pred (car a-list))
               ((list-of pred) (cdr a-list)))))))

(define mark-leaves-with-red-depth
  (lambda (tree)
    (aux tree 0)))

(define aux
  (lambda (tree depth)
    (cases red-blue-tree tree
      (red-node
        (left right)
        (red-node
          (aux left (+ 1 depth))
          (aux right (+ 1 depth))))
      (blue-node
        (children)
        (blue-node
          (map (lambda (child) (aux child depth)) children)))
      (leaf-node
        (int)
        (leaf-node depth)))))
