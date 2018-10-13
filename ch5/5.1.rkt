#lang eopl

; Procedural Representation Of Continuation

; () -> Cont
(define end-cont
    (lambda ()
        (lambda (val)
            (begin
                (eopl:printf "End of computation. ~%s")
                val))))

; Cont -> Cont
(define zero-cont
    (lambda (cont)
        (lambda (val)
            (apply-cont cont (bool-val (zero? (expval->num val)))))))

; Var x Exp x Env x Cont -> Cont
(define let-exp-cont
    (lambda (var body env cont)
        (lambda (val)
            (value-of/k body (extend-env var val env) cont))))

; Exp x Exp x Env x Cont -> Cont
(define if-exp-cont
    (lambda (exp2 exp3 env cont)
        (lambda (val)
            (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))))

; Exp x Env x Cont -> Cont
(define diff1-cont
    (lambda (exp2 env cont)
        (lambda (val)
            (value-of/k exp2 env (diff2-cont val cont)))))

; ExpVal x Cont -> Cont
(define diff2-cont
    (lambda (val1 cont)
        (lambda (val2)
            (apply-cont cont (num-val (- (expval->num val1) (expval->num val2)))))))

; Exp x Env x Cont -> Cont
(define rator-cont
    (lambda (exp2 env cont)
        (lambda (val)
            (value-of/k exp2 env (rand-cont val cont)))))

; ExpVal x Cont -> Cont
(define rand-cont
    (lambda (val1 cont)
        (lambda (val2)
            (apply-procedur/k (expval->proc val1) val2 cont))))

; Cont x ExpVal -> FinalAnswer
(define apply-cont
    (lambda (cont val)
        (cont val)))