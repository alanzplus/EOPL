#lang eopl

(provide identifier?)
(provide a-program)
(provide program)
(provide expression)
(provide expression?)
(provide var-exp)
(provide const-exp)
(provide zero?-exp)
(provide if-exp)
(provide let-exp)
(provide let2-exp)
(provide let3-exp)
(provide list-exp)
(provide diff-exp)
(provide proc-exp)
(provide call-exp)
(provide letrec-exp)
(provide begin-exp)
(provide cons-exp)
(provide car-exp)
(provide cdr-exp)
(provide null?-exp)
(provide emptylist-exp)
(provide letmul-exp)
(provide scanner-spec)
(provide grammer-spec)
(provide just-scan)
(provide scan-parse)
(provide repl-ast)

(define identifier?
  (lambda (x)
    (symbol? x)))

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
   (expression ("let2" identifier "=" expression "," identifier "=" expression "in" expression) let2-exp)
   (expression ("let3" identifier "=" expression "," identifier "=" expression "," identifier "=" expression "in" expression) let3-exp)
   (expression ("proc(" identifier ")" expression ) proc-exp)
   (expression ("(" expression expression ")") call-exp)
   (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
   (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
   (expression ("cons(" expression expression ")") cons-exp)
   (expression ("car(" expression ")") car-exp)
   (expression ("cdr(" expression ")") cdr-exp)
   (expression ("null?(" expression ")") null?-exp)
   (expression ("emptylist") emptylist-exp)
   (expression ("list(" (arbno expression) ")") list-exp)
   (expression ("letmul" (arbno identifier "=" expression) "in" expression) letmul-exp)
  ))

(sllgen:make-define-datatypes scanner-spec grammer-spec)

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammer-spec))

(define scan-parse
  (sllgen:make-string-parser scanner-spec grammer-spec))

(define repl-ast
  (sllgen:make-rep-loop
    "ƛ "
    (lambda (tree) (eopl:pretty-print tree))
    (sllgen:make-stream-parser scanner-spec grammer-spec)))