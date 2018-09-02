#lang eopl

(define subst
  (lambda (new_sym old_sym slst)
    (if (null? slst)
        '()
        (cons
          (subst-in-s-exp new_sym old_sym (car slst))
          (subst new_sym old_sym (cdr slst))))))

(define subst-in-s-exp
  (lambda (new_sym old_sym sexp)
    (if (symbol? sexp)
      (if (eqv? sexp old_sym) new_sym sexp)
      (subst new_sym old_sym sexp))))

(provide subst)
