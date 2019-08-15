#lang racket

(require "monads.rkt")
(provide findf-maybe)
(provide partition-writer)
(provide powerXpartials)
(provide replace-with-count)
(provide reciprocal)
(provide traverse-reciprocal)
(provide halve)
(provide traverse-halve)
(provide state/sum)
(provide traverse-state/sum)
(provide value-of-cps)
(provide empty-env)
(provide extend-env)

(define findf-maybe
  (lambda (predicate? ls)
    (cond
      [(null? ls) (Nothing)]
      [(predicate? (car ls))
       (Just (car ls))]
      [else
        (findf-maybe predicate? (cdr ls))])))

(define partition-writer
  (lambda (predicate? ls)
    (cond
      [(null? ls) (inj-writer '())]
      [(predicate? (car ls))
       (bind-writer
         (partition-writer predicate? (cdr ls))
         (lambda (a)
           (inj-writer (cons (car ls) a))))]
      [else
        (bind-writer
          (tell (car ls))
          (lambda (_)
            (partition-writer predicate? (cdr ls))))])))

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (bind-writer
         (powerXpartials x (sub1 n))
         (lambda (a)
           (Writer (list a) (* a x))))]
      [(even? n)
       (bind-writer
         (powerXpartials x (/ n 2))
         (lambda (a)
           (Writer (list a) (* a a))))])))

(define replace-with-count
  (lambda (s tr)
    (cond
      [(pair? tr)
       (bind-state
         (replace-with-count s (car tr))
         (lambda (a)
           (bind-state
             (replace-with-count s (cdr tr))
             (lambda (b)
               (inj-state (cons a b))))))]
      [(eq? s tr)
       (bind-state
         (get)
         (lambda (s)
           (bind-state
             (put (add1 s))
             (lambda (_)
               (inj-state s)))))]
      [else (inj-state tr)])))

(define reciprocal
  (lambda (n)
    (if (zero? n) (Nothing) (Just (/ 1 n)))))

(define traverse
  (lambda (inj bind f)
    (letrec
      ([trav
         (lambda (tree)
           (cond
             [(pair? tree)
              (go-on ([a (trav (car tree))]
                      [b (trav (cdr tree))])
                     (inj (cons a b)))]
             [else (f tree)]))])
      trav)))

(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))

(define halve
  (lambda (n)
    (if (zero? (modulo n 2))
      (inj-writer (/ n 2))
      (Writer (list n) n))))

(define traverse-halve
  (traverse inj-writer bind-writer halve))

(define state/sum
  (lambda (n)
    (bind-state
      (get)
      (lambda (s)
        (bind-state
          (put (+ s n))
          (lambda (_)
            (inj-state s)))))))

(define traverse-state/sum
  (traverse inj-state bind-state state/sum))

(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (inj-cont expr)]
      [(? boolean?) (inj-cont expr)]
      [(? symbol?) (inj-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-cont
                      (value-of-cps x1 env)
                      (lambda (v1)
                        (bind-cont
                          (value-of-cps x2 env)
                          (lambda (v2)
                            (inj-cont (* v1 v2))))))]
      [`(sub1 ,x) (bind-cont
                    (value-of-cps x env)
                    (lambda (v)
                      (inj-cont (sub1 v))))]
      [`(zero? ,x) (bind-cont
                     (value-of-cps x env)
                     (lambda (v)
                       (inj-cont (zero? v))))]
      [`(if ,test ,conseq ,alt)
        (bind-cont
          (value-of-cps test env)
          (lambda (v1)
            (if v1
              (value-of-cps conseq env)
              (value-of-cps alt env))))]
      [`(lambda (,id) ,body)
        (inj-cont (closure id body env))]
      [`(capture ,k-id ,body)
        (callcc (lambda (k)
                  (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,val-exp)
        (bind-cont
          (value-of-cps k-exp env)
          (lambda (k)
            (bind-cont
              (value-of-cps val-exp env)
              (lambda (v) (k v)))))]
      [`(,rator ,rand)
        (bind-cont
          (value-of-cps rator env)
          (lambda (app)
            (bind-cont
              (value-of-cps rand env)
              (lambda (arg)
                (apply-proc app arg)))))]
      )))

(define apply-proc
  (lambda (app arg)
    (app arg)))

(define closure
  (lambda (var body env)
    (lambda (val)
      (value-of-cps body (extend-env var val env)))))

(define empty-env
  (lambda ()
    (lambda (var)
      (error "No binding for ~s" var))))

(define extend-env
  (lambda (var val env)
    (lambda (search-var)
      (if (eq? var search-var)
        val
        (env search-var)))))

(define apply-env
  (lambda (env var)
    (env var)))
