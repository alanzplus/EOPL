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
(provide diff-exp)
(provide proc-exp)
(provide call-exp)
(provide letrec-exp)
(provide assign-exp)
(provide pair-exp)
(provide left-exp)
(provide right-exp)
(provide setleft-exp)
(provide setright-exp)
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
   (expression ("proc(" identifier ")" expression ) proc-exp)
   (expression ("(" expression expression ")") call-exp)
   (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
   (expression ("set" identifier "=" expression) assign-exp)
   (expression ("pair(" expression "," expression ")") pair-exp)
   (expression ("left(" expression ")") left-exp)
   (expression ("right(" expression ")") right-exp)
   (expression ("setleft(" expression "," expression ")") setleft-exp)
   (expression ("setright(" expression "," expression ")") setright-exp)
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