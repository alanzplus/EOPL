#lang racket

(require rackunit "a1.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A1:"
    (test-suite "countdown"
                (test-equal? "base-case" (countdown 0) (list 0))
                (test-equal? "case1" (countdown 5) (list 5 4 3 2 1 0)))
    (test-suite "insertR"
                (test-equal? "empty-list" (insertR 'x 'y '()) '())
                (test-equal? "no-matched-symbol" (insertR 'x 'y '(a b c)) '(a b c))
                (test-equal? "has-matched-symbol-case-1" (insertR 'x 'y '(x)) '(x y))
                (test-equal? "test-case-from-example" (insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y)))
    (test-suite "insertR-fr"
                (test-equal? "empty-list" (insertR-fr 'x 'y '()) '())
                (test-equal? "no-matched-symbol" (insertR-fr 'x 'y '(a b c)) '(a b c))
                (test-equal? "has-matched-symbol-case-1" (insertR-fr 'x 'y '(x)) '(x y))
                (test-equal? "test-case-from-example" (insertR-fr 'x 'y '(x z z x y x)) '(x y z z x y y x y)))
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
    (test-suite "filter-fr"
                (test-equal? "test-case-from-example1" (filter-fr even? '(1 2 3 4 5 6)) (list 2 4 6)))
    (test-suite "zip"
                (test-equal? "example1" (zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c)))
                (test-equal? "example2" (zip '(1 2 3 4 5 6) '(a b c)) '((1 . a) (2 . b) (3 . c)))
                (test-equal? "example3" (zip '(1 2 3) '(a b c d e f)) '((1 . a) (2 . b) (3 . c))))
    (test-suite "map"
                (test-equal? "example1" (map add1 '(1 2 3 4)) '(2 3 4 5)))
    (test-suite "map-fr"
                (test-equal? "example1" (map-fr add1 '(1 2 3 4)) '(2 3 4 5)))
    (test-suite "append"
                (test-equal? "example1" (append '(a b c) '(1 2 3)) '(a b c 1 2 3)))
    (test-suite "append-fr"
                (test-equal? "example1" (append-fr '(a b c) '(1 2 3)) '(a b c 1 2 3)))
    (test-suite "reverse"
                (test-equal? "example1" (reverse '(a 3 x)) '(x 3 a)))
    (test-suite "reverse-fr"
                (test-equal? "example1" (reverse-fr '(a 3 x)) '(x 3 a)))
    (test-suite "fact"
                (test-equal? "example1" (fact 0) 1)
                (test-equal? "example2" (fact 5) 120))
    (test-suite "fib"
                (test-equal? "example1" (fib 0) 0)
                (test-equal? "example2" (fib 1) 1)
                (test-equal? "example2" (fib 2) 1)
                (test-equal? "example3" (fib 7) 13))
    (test-suite "binary->natural"
                (test-equal? "example1" (binary->natural '()) 0)
                (test-equal? "example2" (binary->natural '(0 0 1)) 4)
                (test-equal? "example3" (binary->natural '(0 0 1 1)) 12)
                (test-equal? "example4" (binary->natural '(1 1 1 1)) 15)
                (test-equal? "example5" (binary->natural '(1 0 1 0 1)) 21)
                (test-equal? "example6" (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))
    (test-suite "binary->natural-fr"
                (test-equal? "example1" (binary->natural-fr '()) 0)
                (test-equal? "example2" (binary->natural-fr '(0 0 1)) 4)
                (test-equal? "example3" (binary->natural-fr '(0 0 1 1)) 12)
                (test-equal? "example4" (binary->natural-fr '(1 1 1 1)) 15)
                (test-equal? "example5" (binary->natural-fr '(1 0 1 0 1)) 21)
                (test-equal? "example6" (binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))
    (test-suite "minus"
                (test-equal? "example1" (minus 5 3) 2)
                (test-equal? "example2" (minus 100 50) 50))
    (test-suite "div"
                (test-equal? "example1" (div 25 5) 5)
                (test-equal? "example2" (div 36 6) 6))
    (test-suite "append-map"
                (test-equal? "example1" (append-map countdown (countdown 5)) '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)))
    (test-suite "append-map-fr"
                (test-equal? "example1" (append-map-fr countdown (countdown 5)) '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)))
    (test-suite "set-difference"
                (test-equal? "example1" (set-difference '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5)))
    (test-suite "set-difference-fr"
                (test-equal? "example1" (set-difference-fr '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5)))
    (test-suite "powerset"
                (test-equal? "example1" (powerset '(3 2 1)) '(() (1) (2) (2 1) (3) (3 1) (3 2) (3 2 1)))
                (test-equal? "example2" (powerset '()) '(())))
    (test-suite "powerset-fr"
                (test-equal? "example1" (powerset-fr '(3 2 1)) '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
                (test-equal? "example2" (powerset-fr '()) '(())))
    (test-suite "cartesian-product"
                (test-equal? "example1" (cartesian-product '(5 4) '(3 2 1)) '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
    (test-suite "cartesian-product-fr"
                (test-equal? "example1" (cartesian-product-fr '(5 4) '(3 2 1)) '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
    (test-suite "collatz"
                (test-equal? "example1" (collatz 12) 1)
                (test-equal? "example2" (collatz 120) 1)
                (test-equal? "example3" (collatz 9999) 1))
))

(run-tests tests)
