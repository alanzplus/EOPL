#lang eopl

; empty-stack
; push
; pop
; top
; empty-stack?

(define unknown-action 
  (lambda (action)
    (eopl:error "unkonw action ~s" action)))

(define empty-stack
  (lambda ()
    (lambda (action)
      (cond ((eqv? 'top action) 
             (eopl:error "stack underflow"))
            ((eqv? 'pop action)
             (eopl:error "stack underflow"))
            (else (unknown-action action))))))

(define push
  (lambda (var stack)
    (lambda (action)
      (cond ((eqv? 'top action) var)
            ((eqv? 'pop action) stack)
            (else (unknown-action))))))

(define top
  (lambda (stack)
    (stack 'top)))

(define pop
  (lambda (stack)
    (stack 'pop)))

(provide empty-stack)
(provide push)
(provide pop)
(provide top)
