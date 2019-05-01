#lang racket

(require rackunit "a2.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A2:"
    (test-suite "list-ref" 
                (test-equal? "case1" (list-ref '(a b c) 2) 'c)
                (test-equal? "case2" (list-ref '(a b c) 0) 'a))
    (test-suite "union"
                (test-equal? "case1" (union '() '()) '())
                (test-equal? "case2" (union '(x) '()) '(x))
                (test-equal? "case3" (union '(x) '(x)) '(x))
                (test-equal? "case4" (union '(x y) '(x z)) '(z x y)))
    (test-suite "extend"
                (test-equal? "case1" ((extend 1 even?) 0) #t)
                (test-equal? "case2" ((extend 1 even?) 1) #t)
                (test-equal? "case3" ((extend 1 even?) 2) #t)
                (test-equal? "case4" ((extend 1 even?) 3) #f)
                (test-equal? "case5" (filter (extend 1 even?) '(0 1 2 3 4 5)) '(0 1 2 4))
                (test-equal? "case6" (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5)) '(0 1 2 3 4))
                (test-equal? "case7" (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5)) '(0 1 2 3 4)))
    (test-suite "walk-symbol"
                (test-equal? "case1" (walk-symbol 'a '((a . 5))) 5)
                (test-equal? "case2" (walk-symbol 'a '((b . c) (a . b))) 'c)
                (test-equal? "case3" (walk-symbol 'a '((a . 5) (b . 6) (c . a))) 5)
                (test-equal? "case4" (walk-symbol 'c '((a . 5) (b . (a . c)) (c . a))) 5)
                (test-equal? "case5" (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a))) '((c . a)))
                (test-equal? "case6" (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e))) 5)
                (test-equal? "case7" (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e))) 'f))
    (test-suite "lambda->lumbda"
                (test-suite "case1" (lambda->lumbda 'x) 'x)
                (test-suite "case2" (lambda->lumbda '(lambda (x) x)) '(lumbda (x) x))
                (test-suite "case3" (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a))))))
                (test-suite "case4" (lambda->lumbda '(lambda (lambda) lambda)) '(lumbda (lambda) lambda))
                (test-suite "case5" (lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y))) '((lumbda (lambda) lambda) (lumbda (y) y)))
                (test-suite "case6" (lambda->lumbda '((lambda (x) x) (lambda (x) x))) '((lumbda (x) x) (lumbda (x) x))))
))

(run-tests tests)