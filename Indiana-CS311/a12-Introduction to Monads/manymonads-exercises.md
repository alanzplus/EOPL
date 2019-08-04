# Exercise of A Schemer's View of Monads

## Exercise 1

**Description**

Exercise: In `remberevensXcountevens`, the increment takes place before the tail recursive call, but we are free
to reorder these events. Implement this reordered-events variant by having the body of the sequel become the
first argument to bind state and make the appropriate adjustments to the sequel . Is this new first argument
to bind state a tail call?


**Solution**

```lisp
(define remberevens-countevens-monad-reorder
  (lambda (l)
    (cond
      [(null? l) (unit-state '())]
      [(pair? (car l))
       (bind-state
         (remberevens-countevens-monad-reorder (car l))
         (lambda (a)
           (bind-state
             (remberevens-countevens-monad-reorder (cdr l))
             (lambda (d)
               (unit-state (cons a d))))))]
      [(or (null? (car l)) (odd? (car l)))
       (bind-state
         (remberevens-countevens-monad-reorder (cdr l))
         (lambda (d)
           (unit-state (cons (car l) d))))]
      [else
        (bind-state
          (remberevens-countevens-monad-reorder (cdr l))
          (lambda (_)
            (lambda (s)
              `(,_ . ,(add1 s)))))])))
```

## Exercise 2

**Description**

Exercise: Define `remberevensXmaxseqevens`, which removes all the evens, but while it does that, it also
returns the length of the longest sequence of even numbers without an odd number. There are two obvious
ways to implement this function; try to implement them both. Hint: Consider holding more than a single
value in the state.

**Solution**

```lisp
; Remove all the evens and also returns the length
; of the longest sequence of even numbers without odd number
(define remvevens-maxsequence
  (letrec ([inc-count (lambda (s)
                        (let ([cnt (car s)]
                              [curr-max (cdr s)])
                          `(_ . (,(add1 cnt) . ,curr-max))))]
           [update-max (lambda (s)
                         (let ([cnt (car s)]
                               [curr-max (cdr s)])
                           `(_ . (0 . ,(max cnt curr-max)))))]
           [unit-state (lambda (a)
                         (lambda (s)
                           `(,a . ,s)))]
           [bind-state (lambda (ma sequel)
                         (lambda (s)
                           (let* ([p (ma s)]
                                  [a^ (car p)]
                                  [s^ (cdr p)]
                                  [mb (sequel a^)])
                             (mb s^))))]
           [fun (lambda (l)
                  (cond [(null? l)
                         (bind-state
                           update-max
                           (lambda (_) (unit-state '())))]
                        [(pair? (car l))
                         (bind-state
                           update-max
                           (lambda (_)
                             (bind-state
                               (fun (car l))
                               (lambda (a)
                                 (bind-state
                                   (fun (cdr l))
                                   (lambda (b)
                                     (unit-state (cons a b))))))))]
                        [(or (null? (car l)) (odd? (car l)))
                         (bind-state
                           update-max
                           (lambda (_)
                             (bind-state
                               (fun (cdr l))
                               (lambda (d) (unit-state (cons (car l) d))))))]
                        [else
                          (bind-state
                            inc-count
                            (lambda (_) (fun (cdr l))))]))])
    (lambda (l)
      (let ([ans ((fun l) '(0 . 0))])
        `(,(car ans) . ,(cddr ans))))))
```
