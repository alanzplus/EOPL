#lang eopl

; see letproc in parser/let-spec.rkt
; see ch3/let-interpreter.rkt

(require "let-interpreter.rkt")

(eopl:pretty-print
  (run "let f = proc (x) proc (y) -(x,-(0,y)) in ((f 3) 4)"))
