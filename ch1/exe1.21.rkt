#lang eopl

(define product
  (lambda (sos1 sos2)
    (cond
      ((null? sos1) '())
      (else
        (append (product-single (car sos1) sos2)
              (product (cdr sos1) sos2))))))

(define product-single
  (lambda (s slist)
    (if (null? slist)
       '()
       (cons (list s (car slist)) (product-single s (cdr slist))))))

(provide product)
