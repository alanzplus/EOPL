#lang eopl

(provide identifier?)
(provide a-program)
(provide const-exp)
(provide var-exp)
(provide diff-exp)
(provide zero?-exp)
(provide proc-exp)
(provide let-exp)
(provide letrec-exp)
(provide if-exp)
(provide call-exp)
(provide Program)
(provide Expression)
(provide scan-parse)
(provide repl-ast)

(define identifier?
  (lambda (x)
    (symbol? x)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)))

(define grammer-spec
  '((Program (Expression) a-program)
    (Expression (identifier) var-exp)
    (Expression (number) const-exp)
    (Expression ("-" "(" Expression "," Expression ")") diff-exp)
    (Expression ("zero?" "(" Expression ")") zero?-exp)
    (Expression ("proc" "(" (separated-list identifier ",") ")" Expression) proc-exp)
    (Expression ("let" identifier "=" Expression "in" Expression) let-exp)
    (Expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" Expression) "in" Expression) letrec-exp)
    (Expression ("if" Expression "then" Expression "else" Expression) if-exp)
    (Expression ("(" Expression (arbno Expression) ")") call-exp)))

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
