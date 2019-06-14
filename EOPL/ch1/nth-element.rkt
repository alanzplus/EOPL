#lang eopl

(define nth-element
  (lambda (lst n)
    (if (null? lst)
      (report-list-tool-short n)
      (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1))))))

(define report-list-tool-short
  (lambda (n)
    (eopl:error 'nth-element
      "List too short by ~s elements.~%" (+ n 1))))

(provide nth-element)

