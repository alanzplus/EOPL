(require "parenthec.rkt")

(define-registers n k k^ v)
(define-program-counter pc)

(define-union kt
              (empty_k dismount)
              (extend n k))

(define-label apply_k
              (union-case k^ kt
                          [(empty_k dismount) (dismount-trampoline dismount)]
                          [(extend n k)
                           (begin
                             (set! k^ k)
                             (set! v (* n v))
                             (set! pc apply_k))]))

(define-label factorial_cps
    (cond
      [(zero? n)
       (begin
         (set! k^ k)
         (set! v 1)
         (set! pc apply_k))]
      [else 
        (begin
          (set! k (kt_extend n k))
          (set! n (sub1 n))
          (set! pc factorial_cps))]))

(define-label main
              (begin
                (set! n 5)
                (set! pc factorial_cps)
                (mount-trampoline kt_empty_k k pc)
                (printf "Factorial of 5: ~a\n" v)))
