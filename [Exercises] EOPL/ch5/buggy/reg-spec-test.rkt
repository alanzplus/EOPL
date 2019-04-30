#lang eopl

(require rackunit "reg-spec.rkt")
(require rackunit/text-ui)

(define cp-spec-test
  (test-suite
    "cp-spec-test"
    (check-equal?
      (scan-parse "a")
      (a-program (var-exp 'a)))
    (check-equal?
      (scan-parse "3")
      (a-program (const-exp 3)))
    (check-equal?
      (scan-parse "-(a,3)")
      (a-program
        (diff-exp
          (var-exp 'a)
          (const-exp 3))))
    (check-equal?
      (scan-parse "zero?(3)")
      (a-program (zero?-exp (const-exp 3))))
    (check-equal?
      (scan-parse "if zero?(3) then 3 else 0")
      (a-program
        (if-exp (zero?-exp (const-exp 3)) (const-exp 3) (const-exp 0))))
    (check-equal?
      (scan-parse "let x = proc(x) x in (x 10)")
      (a-program
        (let-exp
          'x
          (proc-exp (list 'x) (var-exp 'x))
          (call-exp (var-exp 'x) (list (const-exp 10))))))
    (check-equal?
      (scan-parse "letrec rec(x y) = (rec x y) in (rec 10 11)")
      (a-program
        (letrec-exp
          'rec
          (list 'x 'y)
          (call-exp (var-exp 'rec) (list (var-exp 'x) (var-exp 'y)))
          (call-exp (var-exp 'rec) (list (const-exp 10) (const-exp 11))))))
    (check-equal?
      (scan-parse "begin a end")
      (a-program
        (begin-exp (var-exp 'a) '())))
    (check-equal?
      (scan-parse "let2 a = b , c = 3 in -(a,c)")
      (a-program
        (let2-exp 'a (var-exp 'b) 'c (const-exp 3) (diff-exp (var-exp 'a) (var-exp 'c)))))
    (check-equal?
      (scan-parse "let3 a = b , c = 3 , d = 10 in -(-(a,c), d)")
      (a-program
        (let3-exp 'a (var-exp 'b) 'c (const-exp 3) 'd (const-exp 10) (diff-exp (diff-exp (var-exp 'a) (var-exp 'c)) (var-exp 'd)))))
    (check-equal?
      (scan-parse
        "begin
          a;
          b
        end")
      (a-program (begin-exp (var-exp 'a) (list (var-exp 'b)))))
    (check-equal?
      (scan-parse "let l = cons(1 cons(2 emptylist)) in cons(car(l) cdr(l))")
      (a-program
        (let-exp 'l
                 (cons-exp (const-exp 1) (cons-exp (const-exp 2) (emptylist-exp)))
                 (cons-exp (car-exp (var-exp 'l)) (cdr-exp (var-exp 'l))))))
    (check-equal?
      (scan-parse "list(1 2 3)")
      (a-program
        (list-exp (list (const-exp 1) (const-exp 2) (const-exp 3)))))
    (check-equal?
      (scan-parse "letmul in a")
      (a-program
        (letmul-exp '() '() (var-exp 'a))))
    (check-equal?
      (scan-parse "letmul a = 1 b = 2 in -(a,b)")
      (a-program
        (letmul-exp (list 'a 'b) (list (const-exp 1) (const-exp 2)) (diff-exp (var-exp 'a) (var-exp 'b)))))
    (check-equal?
      (scan-parse "set a = 3")
      (a-program
        (assign-exp 'a (const-exp 3))))
))

(run-tests cp-spec-test)