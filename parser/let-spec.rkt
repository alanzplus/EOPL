#lang eopl

(provide let-scanner-spec)
(provide let-grammer-spec)
(provide let-just-scan)
(provide let-scan-parse)
(provide let-read-print-ast)
(provide program)
(provide a-program)
(provide expression)
(provide var-exp)
(provide const-exp)
(provide zero?-exp)
(provide if-exp)
(provide let-exp)
(provide add-exp)
(provide diff-exp)
(provide mul-exp)
(provide div-exp)
(provide minus-exp)
(provide equal?-exp)
(provide greater?-exp)
(provide less?-exp)
(provide emptylist-exp)
(provide cons-exp)
(provide null?-exp)
(provide car-exp)
(provide cdr-exp)
(provide list-exp)
(provide proc-exp)
(provide call-exp)
(provide letproc-exp)
(provide letrec-exp)

(define let-scanner-spec
  '(
    (white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)
  ))

(define let-grammer-spec
 '(
   (program (expression) a-program)
   (expression (identifier) var-exp)
   (expression (number) const-exp)
   (expression ("-" "(" expression "," expression ")") diff-exp)
   (expression ("+" "(" expression "," expression ")") add-exp)
   (expression ("*" "(" expression "," expression ")") mul-exp)
   (expression ("/" "(" expression "," expression ")") div-exp)
   (expression ("equal?(" expression "," expression ")") equal?-exp)
   (expression ("greater?(" expression "," expression ")") greater?-exp)
   (expression ("less?(" expression "," expression ")") less?-exp)
   (expression ("zero?" "(" expression ")") zero?-exp)
   (expression ("if" expression "then" expression "else" expression) if-exp)
   (expression ("let" identifier "=" expression "in" expression) let-exp)
   (expression ("minus(" expression ")") minus-exp)
   (expression ("emptylist") emptylist-exp)
   (expression ("cons(" expression "," expression ")") cons-exp)
   (expression ("null?(" expression ")") null?-exp)
   (expression ("car(" expression ")") car-exp)
   (expression ("cdr(" expression ")") cdr-exp)
   (expression ("list(" (separated-list expression ",") ")") list-exp)
   (expression ("proc (" (arbno identifier) ")" expression ) proc-exp)
   (expression ("(" expression (arbno expression) ")") call-exp)
   (expression ("letproc" identifier "= (" (arbno identifier) ")" expression "in" expression) letproc-exp)
   (expression ("letrec" identifier "(" (arbno identifier) ") = " expression "in" expression) letrec-exp)
  ))

(sllgen:make-define-datatypes let-scanner-spec let-grammer-spec)

(define let-just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammer-spec))

(define let-scan-parse
  (sllgen:make-string-parser let-scanner-spec let-grammer-spec))

(define let-read-print-ast
  (sllgen:make-rep-loop
    "ƛ "
    (lambda (tree) (eopl:pretty-print tree))
    (sllgen:make-stream-parser let-scanner-spec let-grammer-spec)))
