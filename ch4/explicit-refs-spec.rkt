#lang eopl

(provide scanner-spec)
(provide grammer-spec)
(provide scan)
(provide scan-parse)
(provide read-print-ast)
(provide program)
(provide a-program)
(provide expression)
(provide var-exp)
(provide const-exp)
(provide zero?-exp)
(provide if-exp)
(provide let-exp)
(provide diff-exp)
(provide proc-exp)
(provide call-exp)
(provide expression?)
(provide begin-exp)
(provide newref-exp)
(provide deref-exp)
(provide setref-exp)

(define scanner-spec
  '(
    (white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)
  ))

(define grammer-spec
 '(
   (program (expression) a-program)
   (expression (identifier) var-exp)
   (expression (number) const-exp)
   (expression ("-" "(" expression "," expression ")") diff-exp)
   (expression ("zero?" "(" expression ")") zero?-exp)
   (expression ("if" expression "then" expression "else" expression) if-exp)
   (expression ("let" identifier "=" expression "in" expression) let-exp)
   (expression ("proc (" identifier ")" expression ) proc-exp)
   (expression ("(" expression expression ")") call-exp)
   (expression ("setref(" expression "," expression ")") setref-exp)
   (expression ("deref(" expression ")") deref-exp)
   (expression ("newref(" expression ")") newref-exp)
   (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
  ))

(sllgen:make-define-datatypes scanner-spec grammer-spec)

(define scan
  (sllgen:make-string-scanner scanner-spec grammer-spec))

(define scan-parse
  (sllgen:make-string-parser scanner-spec grammer-spec))

(define read-print-ast
  (sllgen:make-rep-loop
    "ƛ "
    (lambda (tree) (eopl:pretty-print tree))
    (sllgen:make-stream-parser scanner-spec grammer-spec)))
