#lang eopl

(require "./cp-spec.rkt")

(provide run)
(provide num-val)
(provide bool-val)
(provide proc-val)
(provide list-val)
(provide string-val)
(provide expval->num)
(provide expval->bool)
(provide expval->proc)
(provide expval->list)
(provide procedure)
(provide apply-procedure/k)

; -----------------------------------------------------------------------------
; List Utilities
; -----------------------------------------------------------------------------

; Return the first idx which satifies the predicate
(define find-idx
  (lambda (predicate lst)
    (letrec ((helper
                (lambda (idx alst)
                    (if (null? alst)
                        -1
                        (if (predicate (car alst))
                            idx
                            (helper (+ idx 1) (cdr alst)))))))
            (helper 0 lst))))

; -----------------------------------------------------------------------------
; Expression Value Representation
; -----------------------------------------------------------------------------
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?))
  (list-val
    (alist list?))
  (string-val
    (str string?))
  (mutex-val
    (mu mutex?))
)

; ExpVal -> num
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error "expected num-val")))))

; ExpVal -> bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error "expected bool-val")))))

; ExpVal -> Procedure
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (eopl:error "expected proc-val")))))

; ExpVal -> list
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (alist) alist)
      (else (eopl:error "expected list-val")))))

; ExpVal -> string
(define expval->string
  (lambda (val)
    (cases expval val
      (string-val (str) str)
      (else (eopl:error "expected string-val")))))

(define expval->mutex
  (lambda (val)
    (cases expval val
      (mutex-val (mu) mu)
      (else (eopl:error "expected mutex-val")))))

; -----------------------------------------------------------------------------
; Procedure Representation
; -----------------------------------------------------------------------------
(define-datatype proc proc?
  (procedure
    (vars (list-of identifier?))
    (body expression?)
    (env environment?)))

; Proc x ExpVal x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
      (procedure (vars body env)
        (if (eqv? (length vars) (length vals))
            (value-of/k body (extend-env-list vars (newref-list vals) env) cont)
            (apply-cont (raise-cont cont) (string-val "procedure call with wrong number of arguments")))))))

; -----------------------------------------------------------------------------
; Store
; -----------------------------------------------------------------------------
(define the-store 'uninitialized)

; () -> Store
(define empty-store
  (lambda () '()))

; () -> Store
(define get-store
  (lambda () the-store))

; () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

; SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))

; ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

; ExpVals -> Refs
(define newref-list
  (lambda (vals)
    (map newref vals)))

; Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

; Ref x ExpVal -> Unspecified
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec
        ((setref-inner
          (lambda (store1 ref1)
            (cond
              ((null? store1)
                (eopl:error "cannot set ~s in reference ~s" val ref))
              ((zero? ref1)
               (cons val (cdr store1)))
              (else
                (cons
                  (car store1)
                  (setref-inner
                    (cdr store1)
                    (- ref1 1))))))))
        (setref-inner the-store ref)))))

; -----------------------------------------------------------------------------
; Environment
; -----------------------------------------------------------------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val reference?)
    (env environment?))
  (extend-env-list
    (vars (list-of identifier?))
    (vals (list-of reference?))
    (env environment?))
  (extend-env-rec
    (p-name identifier?)
    (p-vars (list-of identifier?))
    (body expression?)
    (env environment?)))

; Environment x Identifier -> ExpVal
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (eopl:error "there is no binding for ~s" search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))
      (extend-env-list (saved-vars saved-vals saved-env)
        (let ((idx
                (find-idx
                   (lambda (ele)
                      (eqv? ele search-var))
                   saved-vars)))
              (if (eqv? idx -1)
                  (apply-env saved-env search-var)
                  (list-ref saved-vals idx))))
      (extend-env-rec (p-name p-vars p-body saved-env)
        (if (eqv? search-var p-name)
            (newref (proc-val (procedure p-vars p-body env)))
            (apply-env saved-env search-var))))))

; Initilization
(define init-env
  (lambda ()
    (extend-env
      'i (newref (num-val 1))
      (extend-env
        'v (newref (num-val 5))
          (extend-env
            'x (newref (num-val 10))
            (empty-env))))))
; -----------------------------------------------------------------------------
; FIFO Queue
; -----------------------------------------------------------------------------
(define enqueue
  (lambda (queue ele)
    (append queue (list ele))))

(define dequeue
  (lambda (queue f)
    (f (car queue) (cdr queue))))

(define empty-queue
  (lambda ()
    '()))

(define empty?
  (lambda (queue)
    (null? queue)))

; -----------------------------------------------------------------------------
; Thread Scheduler
; -----------------------------------------------------------------------------
(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
      (enqueue the-ready-queue th))))

(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
      the-final-answer
      (dequeue the-ready-queue
        (lambda (first-ready-thread other-ready-threads)
          (set! the-ready-queue other-ready-threads)
          (set! the-time-remaining the-max-time-slice)
          (first-ready-thread))))))

(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))

; -----------------------------------------------------------------------------
; Mutex
; -----------------------------------------------------------------------------
(define-datatype mutex mutex?
  (a-mutex
    (ref-to-closed? reference?)
    (ref-to-wait-queue reference?)))

(define new-mutex
  (lambda ()
    (a-mutex
      (newref #f)
      (newref '()))))

(define wait-for-mutex
  (lambda (mt th)
    (cases mutex mt
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (cond
          ((deref ref-to-closed?)
            (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) th))
            (run-next-thread))
          (else
            (setref! ref-to-closed? #t)
            (th)))))))

(define signal-mutex
  (lambda (mt th)
    (cases mutex mt
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let ((closed? (deref ref-to-closed?))
              (wait-queue (deref ref-to-wait-queue)))
             (if closed?
               (if (empty? wait-queue)
                 (setref! ref-to-closed? #f)
                 (dequeue
                   wait-queue
                   (lambda (first rest)
                     (place-on-ready-queue! first)
                     (setref! ref-to-wait-queue rest))))
               '())
             (th))))))
          
; -----------------------------------------------------------------------------
; Continuation
; -----------------------------------------------------------------------------
(define-datatype continuation continuation?
  (end-cont)
  (end-main-thread-cont)
  (end-subthread-cont)
  (zero-cont
    (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?))
  (mul1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (mul2-cont
    (val expval?)
    (cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    (val expval?)
    (cont continuation?))
  (rator-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (rand-cont
    (env environment?)
    (val expval?)
    (exps (list-of expression?))
    (vals (list-of expval?))
    (cont continuation?))
  (let2-cont1
    (var1 identifier?)
    (var2 identifier?)
    (exp2 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let2-cont2
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont1
    (var1 identifier?)
    (var2 identifier?)
    (exp2 expression?)
    (var3 identifier?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont2
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (var3 identifier?)
    (exp3 expression?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (let3-cont3
    (var1 identifier?)
    (val1 expval?)
    (var2 identifier?)
    (val2 expval?)
    (var3 identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (cons-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (cons-cont2
    (val expval?)
    (cont continuation?))
  (car-cont
    (cont continuation?))
  (cdr-cont
    (cont continuation?))
  (null?-cont
    (cont continuation?))
  (list-first-cont
    (val expval?)
    (cont continuation?))
  (list-rest-cont
    (exps expression?)
    (env environment?)
    (cont continuation?))
  (letmul-cont
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (body expression?)
    (body-eval-env environment?)
    (binding-eval-env environment?)
    (cont continuation?))
  (set-rhs-cont
    (env environment?)
    (var identifier?)
    (cont continuation?))
  (begin-exp-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (try-cont
    (var identifier?)
    (handler-exp expression?)
    (env environment?)
    (cont continuation?))
  (raise-cont
    (cont continuation?))
  (div-cont1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (div-cont2
    (val1 expval?)
    (cont continuation?))
  (spawn-cont
    (cont continuation?))
  (print-cont
    (cont continuation?))
  (wait-cont
    (cont continuation?))
  (signal-cont
    (cont continuation?))
)

(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
      (begin
        (place-on-ready-queue!
          (lambda () (apply-cont cont val)))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation cont
          (end-cont ()
            (begin
              (eopl:printf "---------------------\n")
              (eopl:printf "End of computation. Final answer: \n")
              (eopl:pretty-print val)
              (eopl:printf "---------------------\n\n")
              val))
          (zero-cont (saved-cont)
            (apply-cont saved-cont
              (bool-val (zero? (expval->num val)))))
          (let-exp-cont (var body saved-env saved-cont)
            (value-of/k
              body
              (extend-env var (newref val) saved-env)
              saved-cont))
          (if-test-cont (exp2 exp3 saved-env saved-cont)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont)
              (value-of/k exp3 saved-env saved-cont)))
          (mul1-cont (exp2 env saved-cont)
            (value-of/k
              exp2
              env
              (mul2-cont val saved-cont)))
          (mul2-cont (val1 saved-cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
                (apply-cont saved-cont (num-val (* num1 num2)))))
          (diff1-cont (exp2 env saved-cont)
            (value-of/k
              exp2
              env
              (diff2-cont val saved-cont)))
          (diff2-cont (val1 saved-cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
                (apply-cont saved-cont (num-val (- num1 num2)))))
          (rator-cont (exps env saved-cont)
            (value-of/k
              (car exps)
              env
              (rand-cont env val (cdr exps) '() saved-cont)))
          (rand-cont (env rator exps vals saved-cont)
              (if (null? exps)
                  (apply-procedure/k (expval->proc rator) (append vals (list val)) saved-cont)
                  (value-of/k
                    (car exps)
                    env
                    (rand-cont env rator (cdr exps) (append vals (list val)) saved-cont))))
          (let2-cont1 (var1 var2 exp2 body env saved-cont)
            (value-of/k exp2 env (let2-cont2 var1 val var2 body env saved-cont)))
          (let2-cont2 (var1 val1 var2 body env saved-cont)
            (value-of/k body (extend-env var2 (newref val) (extend-env var1 (newref val1) env)) saved-cont))
          (let3-cont1 (var1 var2 exp2 var3 exp3 body env saved-cont)
            (value-of/k exp2 env (let3-cont2 var1 val var2 var3 exp3 body env saved-cont)))
          (let3-cont2 (var1 val1 var2 var3 exp3 body env saved-cont)
            (value-of/k exp3 env (let3-cont3 var1 val1 var2 val var3 body env saved-cont)))
          (let3-cont3 (var1 val1 var2 val2 var3 body env saved-cont)
            (value-of/k body (extend-env var3 (newref val) (extend-env var2 (newref val2) (extend-env var1 (newref val1) env))) saved-cont))
          (cons-cont1 (exp2 env saved-cont)
            (value-of/k exp2 env (cons-cont2 val saved-cont)))
          (cons-cont2 (val1 saved-cont)
            (apply-cont saved-cont (list-val (list val1 val))))
          (car-cont (saved-cont)
            (apply-cont saved-cont (car (expval->list val))))
          (cdr-cont (saved-cont)
            (apply-cont saved-cont (list-val (cdr (expval->list val)))))
          (null?-cont (saved-cont)
            (cases expval val
              (list-val (alist) (apply-cont saved-cont (bool-val (null? alist))))
              (else (eopl:error "expected list-val " val))))
          (list-rest-cont (exps env saved-cont)
              (value-of/k exps env (list-first-cont val saved-cont)))
          (list-first-cont (val1 saved-cont)
              (apply-cont saved-cont (list-val (cons val1 (expval->list val)))))
          (letmul-cont (vars exps body body-eval-env binding-eval-env saved-cont)
              (if (null? exps)
                  (value-of/k body (extend-env (car vars) (newref val) body-eval-env) saved-cont)
                  (value-of/k
                    (car exps)
                    binding-eval-env
                    (letmul-cont
                      (cdr vars)
                      (cdr exps)
                      body
                      (extend-env (car vars) (newref val) body-eval-env)
                      binding-eval-env
                      saved-cont))))
          (set-rhs-cont (env var saved-cont)
            (apply-cont saved-cont (setref! (apply-env env var) val)))
          (begin-exp-cont (other-exps env saved-cont)
            (if (null? other-exps)
                (apply-cont saved-cont val)
                (value-of/k
                  (car other-exps)
                  env
                  (begin-exp-cont (cdr other-exps) env saved-cont))))
          (try-cont (var handler-exp env saved-cont)
            (apply-cont saved-cont val))
          (raise-cont (saved-cont)
            (apply-handler val saved-cont))
          (div-cont1 (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env (div-cont2 val saved-cont)))
          (div-cont2 (val1 saved-cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
                (if (eqv? num2 0)
                    (apply-cont (raise-cont saved-cont) (string-val "division by zero"))
                    (apply-cont saved-cont (num-val (/ num1 num2))))))
          (spawn-cont (saved-cont)
            (let ((proc1 (expval->proc val)))
              (place-on-ready-queue!
                (lambda ()
                  (apply-procedure/k
                    proc1
                    (list (num-val 28))
                    (end-subthread-cont))))
              (apply-cont saved-cont (num-val 73))))
          (end-main-thread-cont ()
            (set-final-answer! val)
            (run-next-thread))
          (end-subthread-cont ()
            (run-next-thread))
          (print-cont (saved-cont)
            (begin
              (eopl:pretty-print val)
              (apply-cont saved-cont val)))
          (wait-cont (saved-cont)
            (wait-for-mutex
              (expval->mutex val)
              (lambda () (apply-cont saved-cont (num-val 52)))))
          (signal-cont (saved-cont)
            (signal-mutex
              (expval->mutex val)
              (lambda () (apply-cont saved-cont (num-val 53)))))
          (else (eopl:error "unkonw type of continuation. ~s" cont)))))))

(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      (try-cont (var handler-exp env saved-cont)
        (value-of/k
          handler-exp
          (extend-env var (newref val) env)
          saved-cont))
      ; for unit-testing
      (end-cont ()
        (begin
          (eopl:pretty-print "uncaught exception" ) val))
      (zero-cont (saved-cont)
        (apply-handler val saved-cont))
      (let-exp-cont (var body saved-env saved-cont)
        (apply-handler val saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (apply-handler val saved-cont))
      (mul1-cont (exp2 env saved-cont)
        (apply-handler val saved-cont))
      (mul2-cont (val1 saved-cont)
        (apply-handler val saved-cont))
      (diff1-cont (exp2 env saved-cont)
        (apply-handler val saved-cont))
      (diff2-cont (val1 saved-cont)
        (apply-handler val saved-cont))
      (rator-cont (exp2 env saved-cont)
        (apply-handler val saved-cont))
      (rand-cont (env rator exps vals saved-cont)
        (apply-handler val saved-cont))
      (let2-cont1 (var1 var2 exp2 body env saved-cont)
        (apply-handler val saved-cont))
      (let2-cont2 (var1 val1 var2 body env saved-cont)
        (apply-handler val saved-cont))
      (let3-cont1 (var1 var2 exp2 var3 exp3 body env saved-cont)
        (apply-handler val saved-cont))
      (let3-cont2 (var1 val1 var2 var3 exp3 body env saved-cont)
        (apply-handler val saved-cont))
      (let3-cont3 (var1 val1 var2 val2 var3 body env saved-cont)
        (apply-handler val saved-cont))
      (cons-cont1 (exp2 env saved-cont)
        (apply-handler val saved-cont))
      (cons-cont2 (val1 saved-cont)
        (apply-handler val saved-cont))
      (car-cont (saved-cont)
        (apply-handler val saved-cont))
      (cdr-cont (saved-cont)
        (apply-handler val saved-cont))
      (null?-cont (saved-cont)
        (apply-handler val saved-cont))
      (list-rest-cont (exps env saved-cont)
        (apply-handler val saved-cont))
      (list-first-cont (val1 saved-cont)
        (apply-handler val saved-cont))
      (letmul-cont (vars exps body body-eval-env binding-eval-env saved-cont)
        (apply-handler val saved-cont))
      (set-rhs-cont (env var saved-cont)
        (apply-handler val saved-cont))
      (begin-exp-cont (other-exps env saved-cont)
        (apply-handler val saved-cont))
      (raise-cont (saved-cont)
        (apply-handler val saved-cont))
      (div-cont1 (exp2 saved-env saved-cont)
        (apply-handler val saved-cont))
      (div-cont2 (val1 saved-cont)
        (apply-cont val saved-cont))
      (spawn-cont (saved-cont)
        (apply-cont val saved-cont))
      (print-cont (saved-cont)
        (apply-cont val saved-cont))
      (wait-cont (saved-cont)
        (apply-cont val saved-cont))
      (signal-cont (saved-cont)
        (apply-cont val saved-cont))
      (end-main-thread-cont ()
        (begin
          (eopl:pretty-print "uncaught exception" ) val))
      (end-subthread-cont ()
        (begin
          (eopl:pretty-print "uncaught exception" ) val)))))

; -----------------------------------------------------------------------------
; Interpreter
; -----------------------------------------------------------------------------

; String -> ExpVal
(define run
  (lambda (text)
    (value-of-program (scan-parse text))))

; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (initialize-store!)
        (initialize-scheduler! 10)
        (value-of/k exp1 (init-env) (end-main-thread-cont))))))

; Expression X Environemnt x Continutation -> FinalAnswer (ExpVal)
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (proc-exp (vars body)
        (apply-cont cont (proc-val (procedure vars body env))))
      (letrec-exp (p-name p-vars p-body letrec-body)
        (value-of/k
          letrec-body
          (extend-env-rec p-name p-vars p-body env)
          cont))
      (zero?-exp (exp1)
        (value-of/k exp1 env (zero-cont cont)))
      (let-exp (var exp1 body)
        (value-of/k
          exp1
          env
          (let-exp-cont var body env cont)))
      (if-exp (exp1 exp2 exp3)
        (value-of/k
          exp1
          env
          (if-test-cont exp2 exp3 env cont)))
      (mul-exp (exp1 exp2)
        (value-of/k
          exp1
          env
          (mul1-cont exp2 env cont)))
      (diff-exp (exp1 exp2)
        (value-of/k
          exp1
          env
          (diff1-cont exp2 env cont)))
      (call-exp (exp1 exps)
        (value-of/k
          exp1
          env
          (rator-cont exps env cont)))
      (let2-exp (var1 exp1 var2 exp2 body)
        (value-of/k
          exp1 env (let2-cont1 var1 var2 exp2 body env cont)))
      (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
        (value-of/k
          exp1 env (let3-cont1 var1 var2 exp2 var3 exp3 body env cont)))
      (cons-exp (exp1 exp2)
        (value-of/k exp1 env (cons-cont1 exp2 env cont)))
      (car-exp (exp1)
        (value-of/k exp1 env (car-cont cont)))
      (cdr-exp (exp1)
        (value-of/k exp1 env (cdr-cont cont)))
      (null?-exp (exp1)
        (value-of/k exp1 env (null?-cont cont)))
      (emptylist-exp ()
        (apply-cont cont (list-val '())))
      (list-exp (exps)
        (if (null? exps)
            (apply-cont cont (list-val '()))
            (value-of/k
              (car exps)
              env
              (list-rest-cont (list-exp (cdr exps)) env cont))))
      (letmul-exp (vars exps body)
        (value-of/k
          (car exps)
          env
          (letmul-cont vars (cdr exps) body env env cont)))
      (assign-exp (var exp1)
          (value-of/k
            exp1 env (set-rhs-cont env var cont)))
      (begin-exp (exp-first other-exps)
          (value-of/k
            exp-first
            env
            (begin-exp-cont other-exps env cont)))
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env (try-cont var handler-exp env cont)))
      (raise-exp (exp1)
        (value-of/k exp1 env (raise-cont cont)))
      (div-exp (exp1 exp2)
        (value-of/k exp1 env (div-cont1 exp2 env cont)))
      (spawn-exp (exp1)
        (value-of/k exp1 env (spawn-cont cont)))
      (print-exp (exp1)
        (value-of/k exp1 env (print-cont cont)))
      (new-mutex-exp ()
        (apply-cont cont (mutex-val (new-mutex))))
      (wait-exp (exp1)
        (value-of/k exp1 env (wait-cont cont)))
      (signal-exp (exp1)
        (value-of/k exp1 env (signal-cont cont)))
      (yield-exp ()
        (place-on-ready-queue!
          (lambda () (apply-cont cont (num-val 99))))
        (run-next-thread))
      (else (eopl:error "cannot handle expression: ~s" exp))
)))