#lang racket

(require rackunit "a4.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A4:"
              (test-suite "lex"
                          (test-equal? "case1" (lex '((lambda (x) x) 5)  '()) '((lambda (var 0)) (const 5)))
                          (test-equal? "case2"
                                      (lex '(lambda (!)
                                              (lambda (n)
                                                (if (zero? n) 1 (* n (! (sub1 n))))))
                                           '())
                                      '(lambda
                                         (lambda
                                           (if (zero? (var 0))
                                             (const 1)
                                             (* (var 0) ((var 1) (sub1 (var 0))))))))
                          (test-equal? "case3"
                                      (lex '(let ((! (lambda (!)
                                                       (lambda (n)
                                                         (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
                                              ((! !) 5))
                                           '())
                                      '(let (lambda
                                              (lambda
                                                (if (zero? (var 0))
                                                  (const 1)
                                                  (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
                                         (((var 0) (var 0)) (const 5)))))

))

(run-tests tests)
