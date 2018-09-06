#lang eopl

(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (append (flatten (car slist)) (flatten (cdr slist)))
            (cons (car slist) (flatten (cdr slist)))))))

(provide flatten)
