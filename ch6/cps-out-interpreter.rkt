#lang eopl

(require "../libs/common.rkt")
(require "./cps-out-spec.rkt")

; -----------------------------------------------------------------------------
; Expression Value Representation
; -----------------------------------------------------------------------------
(define-datatype expval expval?
								 (num-val
									 (num number?))
								 (bool-val
									 (bool boolean?))
								 (proc-val
									 (proc proc?)))

(define-datatype proc proc?
								 (procedure
									 (vars (list-of identifier?))
									 (body TfExpression?)
									 (env environment?)))

; Expval -> num
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

; -----------------------------------------------------------------------------
; Environment
; -----------------------------------------------------------------------------
(define-datatype environment environment?
								 (empty-env)
								 (extend-env
									 (vars (list-of identifier?))
									 (vals (list-of expval?))
									 (env environment?))
								 (extend-env-rec
									 (p-names (list-of identifier?))
									 (p-varss (list-of identifier?))
									 (p-bodies (list-of TfExpression?))
									 (env environment?)))

; Environment x Identifier -> ExpVal
(define apply-env
	(lambda (env search-var)
		(cases environment env
					 (empty-env ()
											(eopl:error "there is no binding for ~s" search-var))
					 (extend-env
						 (vars vals saved-env)
						 (let ((idx (index-of vars search-var)))
							 (if (eqv? idx -1)
								 (apply-env saved-env search-var)
								 (list-ref vals idx))))
					 (extend-env-rec
						 (p-names p-varss p-bodies saved-env)
						 (let ((idx (index-of p-names search-var)))
							 (if (eqv? idx -1)
								 (apply-env saved-env search-var)
								 (proc-val (procedure (list-ref p-varss) (list-ref p-bodies) saved-env))))))))

; -----------------------------------------------------------------------------
; Continutation
; -----------------------------------------------------------------------------
(define-datatype continuation continuation?
								 (end-cont))

(define apply-cont
	(lambda (cont val)
		(cases continuation cont
					 (end-cont () val))))

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
		(cases Program pgm
					 (a-program (exp1)
											(value-of/k exp1 (empty-env) (end-cont))))))

; TfExpression x Environment x Continutation -> FinalAnswer
(define value-of/k
	(lambda (exp env cont)
		(cases TfExpression exp
					 (simple-exp->exp (simple)
														(apply-cont cont (value-of-simple-exp simple env)))
					 (cps-let-exp (var rhs body)
												(let ((val (value-of-simple-exp rhs env)))
													(value-of/k
														body
														(extend-env (list var) (list val) env)
														cont)))
					 (cps-letrec-exp (p-names b-varss p-bodies body)
													 (value-of/k
														 body 
														 (extend-env-rec p-names b-varss p-bodies env)
														 cont))
					 (cps-if-exp (simple1 body1 body2)
											 (if (expval->bool (value-of-simple-exp simple1 env))
												 (value-of/k body1 env cont)
												 (value-of/k body2 env cont)))
					 (cps-call-exp (rator rands)
												 (let ((rator-proc
																 (expval->proc
																	 (value-of-simple-exp rator env)))
															 (rand-vals
																 (map
																	 (lambda (simple)
																		 (value-of-simple-exp simple env))
																	 rands)))
													 (apply-procedure/k rator-proc rand-vals cont))))))

; Procedure x ExpVals -> ExpVal
(define apply-procedure/k
	(lambda (proc1 args cont)
		(cases proc proc1
					 (procedure (vars body saved-env)
											(value-of/k
												body
												(extend-env vars args saved-env)
												cont)))))

; SimpleExpression x Environment -> ExpVal
(define value-of-simple-exp
	(lambda (exp env)
		(cases SimpleExpression exp
					 (var-exp (id) (apply-env env id))
					 (const-exp (num) (num-val num))
					 (cps-diff-exp (simple1 simple2)
												 (let ((val1 (value-of-simple-exp simple1 env))
															 (val2 (value-of-simple-exp simple2 env)))
													 (num-val (- val1 val2))))
					 (cps-zero?-exp (simple1)
													(let ((val (value-of-simple-exp simple1 env)))
														(bool-val (eqv? (expval->num val) 0))))
					 (cps-proc-exp (vars body)
												 (procedure vars body env)))))
