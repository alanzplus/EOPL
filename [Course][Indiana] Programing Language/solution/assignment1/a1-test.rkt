#lang racket

(require rackunit "a1.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A1:"
    (test-suite "countdown"
                (test-equal? "base-case" (countdown 0) (list 0))
                (test-equal? "case1" (countdown 5) (list 5 4 3 2 1 0)))
    (test-suite "insertRt"
                (test-equal? "empty-list" (insertR 'x 'y '()) '())
                (test-equal? "no-matched-symbol" (insertR 'x 'y '(a b c)) '(a b c))
                (test-equal? "has-matched-symbol-case-1" (insertR 'x 'y '(x)) '(x y))
                (test-equal? "test-case-from-example" (insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y)))
    (test-suite "remv-1st"
                (test-equal? "empty-list" (remv-1st 'x '()) '())
                (test-equal? "no-matched-symbol" (remv-1st 'x '(y)) '(y))
                (test-equal? "has-1-matched-symbol" (remv-1st 'x '(x)) '())
                (test-equal? "has-2-matched-symbols" (remv-1st 'x '(x x)) '(x))
                (test-equal? "test-case-from-example1" (remv-1st 'x '(x y z x)) '(y z x))
                (test-equal? "test-case-from-example2" (remv-1st 'y '(x y z y x)) '(x z y x)))
    (test-suite "list-index-ofv?"
                (test-equal? "test-case-from-example1" (list-index-ofv? 'x '(x y z x x)) 0)
                (test-equal? "test-cass-from-example2" (list-index-ofv? 'x '(y z x x)) 2))
    (test-suite "filter"
                (test-equal? "test-case-from-example1" (filter even? '(1 2 3 4 5 6)) (list 2 4 6)))
    (test-suite "zip"
                (test-equal? "example1" (zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c)))
                (test-equal? "example2" (zip '(1 2 3 4 5 6) '(a b c)) '((1 . a) (2 . b) (3 . c)))
                (test-equal? "example3" (zip '(1 2 3) '(a b c d e f)) '((1 . a) (2 . b) (3 . c))))
))

(run-tests tests)
