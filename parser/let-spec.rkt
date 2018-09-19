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
(provide diff-exp)
(provide zero?-exp)
(provide if-exp)
(provide let-exp)

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
   (expression ("zero?" "(" expression ")") zero?-exp)
   (expression ("if" expression "then" expression "else" expression) if-exp)
   (expression ("let" identifier "=" expression "in" expression) let-exp)
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
