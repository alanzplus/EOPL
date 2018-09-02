#lang eopl

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? slist)
          (cond
            ((eqv? s1 slist) s2)
            ((eqv? s2 slist) s1)
            (else slist))
          (cons
            (swapper s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))))
                

(provide swapper)
