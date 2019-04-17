#lang eopl

(provide cps-identifier?)
(provide cps-a-program)
(provide cps-const-exp)
(provide cps-var-exp)
(provide cps-diff-exp)
(provide cps-zero?-exp)
(provide cps-proc-exp)
(provide simple-exp->exp)
(provide cps-let-exp)
(provide cps-letrec-exp)
(provide cps-if-exp)
(provide cps-call-exp)
(provide CPSProgram)
(provide TfExpression)
(provide TfExpression?)
(provide SimpleExpression)
(provide SimpleExpression?)
(provide cps-scan-parse)
(provide cps-repl-ast)

(define cps-identifier?
  (lambda (x)
    (symbol? x)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)))

(define grammer-spec
  '((CPSProgram (TfExpression) cps-a-program)
    (SimpleExpression (identifier) cps-var-exp)
    (SimpleExpression (number) cps-const-exp)
    (SimpleExpression ("-" "(" SimpleExpression "," SimpleExpression ")") cps-diff-exp)
    (SimpleExpression ("zero?" "(" SimpleExpression ")") cps-zero?-exp)
    (SimpleExpression ("proc" "(" (separated-list identifier ",") ")" TfExpression) cps-proc-exp)
    (TfExpression (SimpleExpression) simple-exp->exp)
    (TfExpression ("let" identifier "=" SimpleExpression "in" TfExpression) cps-let-exp)
    (TfExpression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" TfExpression) "in" TfExpression) cps-letrec-exp)
    (TfExpression ("if" SimpleExpression "then" TfExpression "else" TfExpression) cps-if-exp)
    (TfExpression ("(" SimpleExpression (arbno SimpleExpression) ")") cps-call-exp)))

(sllgen:make-define-datatypes scanner-spec grammer-spec)

(define cps-just-scan
  (sllgen:make-string-scanner scanner-spec grammer-spec))

(define cps-scan-parse
  (sllgen:make-string-parser scanner-spec grammer-spec))

(define cps-repl-ast
  (sllgen:make-rep-loop
    "ƛ "
    (lambda (tree) (eopl:pretty-print tree))
    (sllgen:make-stream-parser scanner-spec grammer-spec)))