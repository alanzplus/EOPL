#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "
        let x = 0
          in begin
            set x = yield;
            x
           end")
      (num-val 99))
    (check-equal?
      (run "
        begin
          spawn(
            proc(x)
              begin
                print(1);
                yield;
                print(2)
              end);
          yield;
          spawn(
            proc(x)
              begin
                print(3);
                yield;
                print(4)
              end);
          yield
        end")
      (num-val 99))
))

(run-tests cp-interpreter-test)