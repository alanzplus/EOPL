#lang eopl

(define list-last-index
  (lambda (predicate lst)
    (let helper ([lst lst] [idx 0])
      (if (null? lst)
        #f
        (let ((res (helper (cdr lst) (+ 1 idx))))
          (if res
            res
            (if (predicate (car lst)) idx #f)))))))

(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ([exps exps])
      (let ((pos (list-last-index
                   (lambda (exp)
                     (not (expression-simple? exp)))
                   exps)))
        (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'var)))
            (cps-of-exp
              (list-ref exps pos)
              (cps-proc-exp (list var)
                            (cps-of-rest
                              (list-set exps pos (var-exp var)))))))))))
