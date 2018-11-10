#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "3")
      (num-val 3))
    (check-equal?
      (run "let x = 0 in if zero?(x) then 3 else 4")
      (num-val 3))
    (check-equal?
      (run "-(3,4)")
      (num-val -1))
    (check-equal?
      (run "let x = 3 in -(1,x)")
      (num-val -2))
    (check-equal?
      (run "let f = proc(x) -(x,4) in (f 10)")
      (num-val 6))
    (check-equal?
      (run "let f = proc(x y z) -(-(x,y),z) in (f 10 9 5)")
      (num-val -4))
    (check-equal?
      (run "letrec sum(x) = if zero?(x) then 0 else -((sum -(x,1)), 1) in (sum 5)")
      (num-val -5))
    (check-equal?
      (run "letrec sum(x y) = if zero?(x) then y else (sum -(x,1) -(1,-(0,y))) in (sum 5 1)")
      (num-val 6))
    (check-equal?
      (run "try raise 5 catch (x) x")
      (num-val 5))
    (check-equal?
      (run "
            let index
                = proc(n)
                  letrec inner(lst)
                    = if null?(lst)
                      then raise 99
                      else if zero?(-(car(lst),n)) then 0 else -((inner cdr(lst)), 1)
                    in proc(lst)
                      try (inner lst)
                        catch (x) x
            in ((index 5) list(2 3))")
      (num-val 99))
    (check-equal?
      (run "/(3,1)")
      (num-val 3))
    (check-equal?
      (run "print(3)")
      (num-val 3))
    (check-equal?
      (run "
    let buffer = 0
    in let producer = proc(n)
            letrec
              wait(k) = if zero?(k)
                        then set buffer = n
                        else begin
                              print(-(k,-(0,200)));
                              (wait -(k,1))
                             end
             in (wait 5)
       in let consumer = proc(d)
             letrec busywait(k) = if zero?(buffer)
                                  then begin
                                        print(-(k, -(0, 100)));
                                        (busywait -(k, -(0, 1)))
                                       end
                                  else buffer
                    in (busywait 0)
            in begin
                spawn(proc(d) (producer 44));
                print(300);
                (consumer 86)
              end")
      (num-val 44))
  ))

(run-tests cp-interpreter-test)
