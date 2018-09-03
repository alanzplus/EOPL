#lang eopl

(define count-occurrences
  (lambda (s slist)
    (cond
      ((null? slist) 0)
      ((symbol? slist) (if (eqv? s slist) 1 0))
      (else (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))))))

(provide count-occurrences)
