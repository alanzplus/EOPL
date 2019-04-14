#lang eopl

(provide index-of)
(provide list-index)

; Return the first idx which satifies the predicate otherwise -1 is returned
(define find-index
  (lambda (predicate lst)
          (letrec ((helper
                    (lambda (idx alst)
                            (if (null? alst)
                              -1
                              (if (predicate (car alst))
                                idx
                                (helper (+ idx 1) (cdr alst)))))))
                  (helper 0 lst))))

(define list-index
  (lambda (predicate lst)
    (let ((pos (find-index predicate lst)))
      (if (eqv? pos -1) #f pos))))

; Implement the index-of function in Racket
; API specification see https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
; When not found, -1 is returned
(define index-of
  (lambda (lst search-e)
          (find-index
           (lambda (e)
                   (eqv? e search-e))
           lst)))
