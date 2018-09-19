#lang eopl

(require rackunit "let-spec.rkt")
(require rackunit/text-ui)

(define let-spec-test
  (test-suite
    "Tests for let"
    (check-equal?
      (let-scan-parse "-(55, -(x, 11))")
      (a-program
        (diff-exp
          (const-exp 55)
          (diff-exp
            (var-exp 'x)
            (const-exp 11)))))
    (check-equal?
      (let-scan-parse "let x = 5 in -(x,3)")
      (a-program
        (let-exp
          'x
          (const-exp 5)
          (diff-exp
            (var-exp 'x)
            (const-exp 3)))))
    (check-equal?
      (let-scan-parse "if let x = 5 in -(x,3) then zero?(0) else -(3,4)")
      (a-program
        (if-exp
          (let-exp
            'x
            (const-exp 5)
            (diff-exp (var-exp 'x) (const-exp 3)))
          (zero?-exp (const-exp 0))
          (diff-exp (const-exp 3) (const-exp 4)))))
  ))

(run-tests let-spec-test)
