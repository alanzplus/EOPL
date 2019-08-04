# A Schemer's View of Monads

## Lecture 1

### Examples
Examples rewritten below are not necessary the same as those in the *manymonads*

**unit-state and bind-state**

```lisp
(define unit-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))

(define bind-state
  (lambda (ma sequel)
    (lambda (s)
      (let* ([p (ma s)]
             [a^ (car p)]
             [s^ (cdr p)]
             [mb (sequel a^)])
        (mb s^)))))
```

**even-length?**

```
(define even-length?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [else
        (not (even-length? (cdr ls)))])))

(define even-length?-sps
  (lambda (ls s)
    (cond
      [(null? ls) s]
      [else (even-length?-sps (cdr ls) (not s))])))


(define even-length?-state
  (lambda (ls)
    (cond
      [(null? ls)
       (displayln "null list")
       (unit-state '_)]
      [else
        (bind-state
          (lambda (s)
            `(_ . ,(not s)))
          (lambda (_)
            (even-length?-state (cdr ls))))])))

((even-length?-state '(1 2 3)) #t)
```

**remberevens-countevens**

```lisp
(define remberevens-countevens
  (lambda (l)
    (cond
      [(null? l) '(() . 0)]
      [(pair? (car l))
       (let* ([ans1 (remberevens-countevens (car l))]
              [ans2 (remberevens-countevens (cdr l))])
         `(,(cons (car ans1) (car ans2)) . ,(+ (cdr ans1) (cdr ans2))))]
      [(or (null? (car l)) (odd? (car l)))
       (let ([ans (remberevens-countevens (cdr l))])
         `(,(cons (car l) (car ans)) . ,(cdr ans)))]
      [else (let ([ans (remberevens-countevens (cdr l))])
              `(,(car ans) . ,(add1 (cdr ans))))])))

(define remberevens-countevens-cps
  (lambda (l k)
    (cond
      [(null? l) (k '(() . 0))]
      [(pair? (car l))
       (remberevens-countevens-cps (car l) (lambda (ans1)
                                             (remberevens-countevens-cps (cdr l) (lambda (ans2)
                                                                                   (k `(,(cons (car ans1) (car ans2)) . ,(+ (cdr ans1) (cdr ans2))))))))]
      [(or (null? (car l)) (odd? (car l)))
       (remberevens-countevens-cps (cdr l) (lambda (ans)
                                             (k `(,(cons (car l) (car ans)) . ,(cdr ans)))))]
      [else (remberevens-countevens-cps (cdr l) (lambda (ans)
                                                  (k `(,(car ans) . ,(add1 (cdr ans))))))])))

(remberevens-countevens '(2 3 (7 4 5 6) 8 (9) 2))
(remberevens-countevens-cps '(2 3 (7 4 5 6) 8 (9) 2) (lambda (v) v))

(define remberevens-direct
  (lambda (l)
    (cond
      [(null? l) '()]
      [(pair? (car l))
       (cons (remberevens-direct (car l)) (remberevens-direct (cdr l)))]
      [(or (null? (car l)) (odd? (car l)))
       (cons (car l) (remberevens-direct (cdr l)))]
      [else (remberevens-direct (cdr l))])))

(remberevens-direct '(2 3 (7 4 5 6) 8 (9) 2))

(define remberevens
  (lambda (l)
    (cond
      [(null? l) (unit-state '())]
      [(pair? (car l))
       (bind-state
         (remberevens (car l))
         (lambda (a)
           (bind-state
             (remberevens (cdr l))
             (lambda (d)
               (unit-state (cons a d))))))]
      [(or (null? (car l)) (odd? (car l)))
       (bind-state
         (remberevens (cdr l))
         (lambda (d)
           (unit-state (cons (car l) d))))]
      [else (remberevens (cdr l))])))

((remberevens '(2 3 (7 4 5 6) 8 (9) 2)) 0)

(define remberevens-countevens-monad
  (lambda (l)
    (cond
      [(null? l) (unit-state '())]
      [(pair? (car l))
       (bind-state
         (remberevens-countevens-monad (car l))
         (lambda (a)
           (bind-state
             (remberevens-countevens-monad (cdr l))
             (lambda (d)
               (unit-state (cons a d))))))]
      [(or (null? (car l)) (odd? (car l)))
       (bind-state
         (remberevens-countevens-monad (cdr l))
         (lambda (d)
           (unit-state (cons (car l) d))))]
      [else
        (bind-state
          (lambda (s)
            `(_ . ,(add1 s)))
          (lambda (_) (remberevens-countevens-monad (cdr l))))])))

((remberevens-countevens-monad '(2 3 (7 4 5 6) 8 (9) 2)) 0)
```

### Exercises

#### Exercise 1

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

#### Exercise 2

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
