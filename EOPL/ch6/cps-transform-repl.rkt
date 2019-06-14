#lang eopl
(require "cps-transform.rkt")
(require "cps-in-spec.rkt")
(require "cps-out-interpreter.rkt")

(define repl
  (create-repl-with
    (lambda (tree)
      (let [(ast (cps-of-program tree))]
        (eopl:printf "------- Transformed AST: -------\n")
        (eopl:pretty-print ast)
        (eopl:printf "\n\n")
        (eopl:printf "------- Evaluation Result: -------\n")
        (eopl:pretty-print (cps-out-value-of-program ast))
        (eopl:printf "\n\n")
        ))))

(repl)
