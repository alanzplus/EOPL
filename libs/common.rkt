#lang eopl

(provide index-of)

; Return the first idx which satifies the predicate otherwise -1 is returned
(define find-idx
  (lambda (predicate lst)
          (letrec ((helper
                    (lambda (idx alst)
                            (if (null? alst)
                              -1
                              (if (predicate (car alst))
                                idx
                                (helper (+ idx 1) (cdr alst)))))))
                  (helper 0 lst))))

; Implement the index-of function in Racket
; API specification see https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
; When not found, -1 is returned
(define index-of
  (lambda (lst search-e)
          (find-idx
           (lambda (e)
                   (eqv? e search-e))
           lst)))