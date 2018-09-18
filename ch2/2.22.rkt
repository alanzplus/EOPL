#lang eopl

; empty-stack -> stack
; push var x stack  -> stack
; pop stack -> stack
; top stack -> var
; empty-stack? -> boolean

(provide empty-stack)
(provide empty-stack?)
(provide push)
(provide top)
(provide pop)

(define value?
  (lambda (v) #t))

(define-datatype stack stack?
  (empty-stack)
  (push
    (var value?)
    (old_stack stack?)))

(define top
  (lambda (a-stack)
    (cases stack a-stack
      (empty-stack () (eopl:error "stack underflow"))
      (push (var old_stack) var))))

(define pop
  (lambda (a-stack)
    (cases stack a-stack
      (empty-stack () (eopl:error "stack underflow"))
      (push (var old_stack) old_stack))))

(define empty-stack?
  (lambda (a-stack)
    (cases stack a-stack
      (empty-stack () #t)
      (else #f))))
