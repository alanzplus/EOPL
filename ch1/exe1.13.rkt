#lang eopl

(define subst
  (lambda (new_symbol old_symbol slist)
    (map (lambda (sexp)
          (if (symbol? sexp)
            (if (eqv? old_symbol sexp) new_symbol sexp)
            (subst new_symbol old_symbol sexp)))
          slist)))

(provide subst)
