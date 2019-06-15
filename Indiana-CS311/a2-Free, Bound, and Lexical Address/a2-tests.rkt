#lang racket

(require rackunit "a2.rkt")
(require rackunit/text-ui)

(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))

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
                (test-equal? "case1" (lambda->lumbda 'x) 'x)
                (test-equal? "case2" (lambda->lumbda '(lambda (x) x)) '(lumbda (x) x))
                (test-equal? "case3" (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a))))))
                (test-equal? "case4" (lambda->lumbda '(lambda (lambda) lambda)) '(lumbda (lambda) lambda))
                (test-equal? "case5" (lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y))) '((lumbda (lambda) lambda) (lumbda (y) y)))
                (test-equal? "case6" (lambda->lumbda '((lambda (x) x) (lambda (x) x))) '((lumbda (x) x) (lumbda (x) x))))
    (test-suite "var-occurs"
                (test-equal? "case1" (var-occurs? 'x 'x) #t)
                (test-equal? "case2" (var-occurs? 'x '(lambda (x) y)) #f)
                (test-equal? "case3" (var-occurs? 'x '(lambda (y) x)) #t)
                (test-equal? "case4" (var-occurs? 'x '((z y) x)) #t)
                (test-equal? "case5" (var-occurs? 'x '(lambda (x) x)) #t))
    (test-suite "vars"
                (test-equal? "case1" (vars 'x) '(x))
                (test-equal? "case1" (vars '(lambda (x) x)) '(x))
                (test-equal? "case1" (vars '((lambda (y) (x x)) (x y))) '(x x x y))
                (test-equal? "case1" (vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(a z h h a)))
    (test-suite "vars-unique"
                (test-equal? "case1" (unique-vars '((lambda (y) (x x)) (x y))) '(y x))
                (test-equal? "case1" (unique-vars '((lambda (z) (lambda (y) (z y))) x)) '(x y z))
                (test-equal? "case1" (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a)))) '(c b a)))
    (test-suite "var-occurs-free?"
                (test-equal? "case1" (var-occurs-free? 'x 'x) #t)
                (test-equal? "case2" (var-occurs-free? 'x '(lambda (y) y)) #f)
                (test-equal? "case3" (var-occurs-free? 'x '(lambda (x) (x y))) #f)
                (test-equal? "case4" (var-occurs-free? 'x '(lambda (x) (lambda (x) x)))  #f)
                (test-equal? "case5" (var-occurs-free? 'y '(lambda (x) (x y))) #t)
                (test-equal? "case6" (var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y)))) #t)
                (test-equal? "case7" (var-occurs-free? 'x '((lambda (x) (x x)) (x x))) #t))
    (test-suite "var-occurs-bound?"
                (test-equal? "case1" (var-occurs-bound? 'x 'x) #f)
                (test-equal? "case2" (var-occurs-bound? 'x '(lambda (x) x)) #t)
                (test-equal? "case3" (var-occurs-bound? 'y '(lambda (x) x)) #f)
                (test-equal? "case4" (var-occurs-bound? 'x '((lambda (x) (x x)) (x x))) #t)
                (test-equal? "case5" (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z)))) #f)
                (test-equal? "case6" (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z)))) #t)
                (test-equal? "case7" (var-occurs-bound? 'x '(lambda (x) y)) #f)
                (test-equal? "case8" (var-occurs-bound? 'x '(lambda (x) (lambda (x) x))) #t))
    (test-suite "unique-free-vars"
                (test-equal? "case1" (unique-free-vars 'x) '(x))
                (test-equal? "case2" (unique-free-vars '(lambda (x) (x y))) '(y))
                (test-equal? "case3" (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) '(x e y)))
    (test-suite "unique-bound-vars"
                (test-equal? "case1" (unique-bound-vars 'x) '())
                (test-equal? "case2" (unique-bound-vars '(lambda (x) y)) '())
                (test-equal? "case3" (unique-bound-vars '(lambda (x) (x y))) '(x))
                (test-equal? "case4" (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) '(c x))
                (test-equal? "case5" (unique-bound-vars '(lambda (y) y)) '(y))
                (test-equal? "case6" (unique-bound-vars '(lambda (x) (y z))) '())
                (test-equal? "case7" (unique-bound-vars '(lambda (x) (lambda (x) x))) '(x)))
    (test-suite "lex"
                (test-equal? "case1" (lex '(lambda (x) x) '()) '(lambda (var 0)))
                (test-equal? "case2" (lex '(lambda (y) (lambda (x) y)) '()) '(lambda (lambda (var 1))))
                (test-equal? "case3" (lex '(lambda (y) (lambda (x) (x y))) '()) '(lambda (lambda ((var 0) (var 1)))))
                (test-equal? "case4" (lex '(lambda (x) (lambda (x) (x x))) '()) '(lambda (lambda ((var 0) (var 0)))))
                (test-equal? "case5" (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
                (test-equal? "case6"
                             (lex '(lambda (a)
                                     (lambda (b)
                                       (lambda (c)
                                         (lambda (a)
                                           (lambda (b)
                                             (lambda (d)
                                               (lambda (a)
                                                 (lambda (e)
                                                   (((((a b) c) d) e) a))))))))) '())
                             '(lambda
                                (lambda
                                  (lambda
                                    (lambda
                                      (lambda
                                        (lambda
                                          (lambda
                                            (lambda
                                              ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
                (test-equal? "case7"
                             (lex '(lambda (a)
                                     (lambda (b)
                                       (lambda (c)
                                         (lambda (w)
                                           (lambda (x)
                                             (lambda (y)
                                               ((lambda (a)
                                                  (lambda (b)
                                                    (lambda (c)
                                                      (((((a b) c) w) x) y))))
                                                (lambda (w)
                                                  (lambda (x)
                                                    (lambda (y)
                                                      (((((a b) c) w) x) y))))))))))) '())
                             '(lambda 
                                (lambda 
                                  (lambda 
                                    (lambda 
                                      (lambda 
                                        (lambda 
                                          ((lambda
                                             (lambda
                                               (lambda
                                                 ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                                           (lambda
                                             (lambda
                                               (lambda
                                                 ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0))))))))))))))
    (test-suite "walk-symbol-update"
                (walk-symbol-update 'a a-list)
                (test-equal? "case1" a-list '((c . #&15) (e . #&f) (b . #&15) (a . #&15))))
    (test-suite "var-occurs-both?"
                (test-equal? "case1" (call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (x (lambda (x) x))))) cons) (cons #f #t))
                (test-equal? "case2" (call-with-values (lambda () (var-occurs-both? 'x '(x (lambda (x) x)))) cons) (cons #t #t))
                (test-equal? "case3" (call-with-values (lambda () (var-occurs-both? 'x '(lambda (y) (x (lambda (x) x))))) cons)  (cons #t #t))
                (test-equal? "case4" (call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (lambda (x) (x (lambda (x) x)))))) cons) (cons #f #t))
                (test-equal? "case5" (call-with-values (lambda () (var-occurs-both? 'x '(lambda (x) (lambda (y) (lambda (x) (x (lambda (x) x))))))) cons) (cons #f #t))
                (test-equal? "case6" (call-with-values (lambda () (var-occurs-both? 'x '(lambda (y) (lambda (x) (lambda (z) (lambda (x) (x (lambda (x) x)))))))) cons) (cons #f #t)))
))

(run-tests tests)