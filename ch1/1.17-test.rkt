#lang eopl

(require rackunit "exe1.17.rkt")
(require rackunit/text-ui)

(define down-test
  (test-suite
    "Tests for down"
    (check-equal? (down '(1 2 3)) '((1) (2) (3)))
    (check-equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
    (check-equal? 
      (down '(a (more (complicated)) object))
      '((a) ((more (complicated))) (object)))))

(run-tests down-test)
