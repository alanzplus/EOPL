#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for let"
    (check-equal? (expval->list (run "emptylist")) '())
    (check-equal?
      (run "cons(-(1,2),emptylist)")
      (list-val
        (list
          (num-val -1)
          (empty-list-val))))
    (check-equal?
      (run "let x = 4 in cons(x, cons(cons(-(x,2), emptylist), emptylist))")
      (list-val
        (list
          (num-val 4)
          (list-val
            (list (list-val (list (num-val 2) (empty-list-val))) (empty-list-val))))))
    (check-equal?
      (run "null?(emptylist)")
      (bool-val #t))
    (check-equal?
      (run "null?(cons(3,4))")
      (bool-val #f))
    (check-equal?
      (run "car(cons(3,4))")
      (num-val 3))
    (check-equal?
      (run "car(cons(cons(3,4),emptylist))")
      (list-val (list (num-val 3) (num-val 4))))
    (check-equal?
      (run "cdr(cons(3,4))")
      (list-val (list (num-val 4))))
    (check-equal?
      (run "cdr(cons(cons(3,4),emptylist))")
      (list-val (list (empty-list-val))))
  ))

(run-tests let-test)
