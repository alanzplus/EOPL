#lang eopl
(require "cps-transform.rkt")
(require "cps-in-spec.rkt")

(define repl
  (create-repl-with
    (lambda (tree)
      (cps-of-program tree))))

(repl)
