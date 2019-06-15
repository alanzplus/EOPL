#lang racket
(provide (all-defined-out))
(print-as-expression #f)

;; Day 2 Bonus Material! 
;; Our derived version of Ackermann

(define G
  (lambda (i x y)
    (cond
      [(and (zero? y) (zero? i)) x]
      [(and (zero? y) (zero? (sub1 i))) 0]
      [(zero? y) 1]
      [(zero? i) (add1 (G i x (sub1 y)))]
      [else (G (sub1 i) x (G i x (sub1 y)))])))

;; The original phi function as defined by Ackermann himself

(define phi
  (lambda (p m n)
    (cond
      [(zero? p) (+ m n)]
      [(and (zero? n) (zero? (sub1 p))) 0]
      [(and (zero? n) (zero? (sub1 (sub1 p)))) 1]
      [(zero? n) m]
      [else (phi (sub1 p) m (phi p m (sub1 n)))])))


;; After Ackermann's publication of his function (which had three
;; nonnegative integer arguments), many authors modified it to suit
;; various purposes, so that today "the Ackermann function" may refer to
;; any of numerous variants of the original function. One common version,
;; the two-argument Ackermann–Péter function, is defined as follows for
;; nonnegative integers m and n:
;; -- wiki

(define ack-peter
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack-peter (sub1 m) 1)]
      [else (ack-peter (sub1 m) (ack-peter m (sub1 n)))])))

;; As it turns out Ackermann's function was /not/ the first recursive
;; function which is not primitive recursive:
;; http://www.sciencedirect.com/science/article/pii/0315086079900247

;; Nested Version
;; (define sudan
;;   (lambda (i x y)
;;     (cond
;;       [(zero? i) (+ x y)]
;;       [(zero? y) x]
;;       [else (sudan (sub1 i)
;;                    (sudan i x (sub1 y))
;;                    (+ (sudan i x (sub1 y)) y))])))

;; Let binding above.
(define sudan
  (lambda (i x y)
    (cond
      [(zero? i) (+ x y)]
      [(zero? y) x]
      [else (let ((x^ (sudan i x (sub1 y))))
              (sudan (sub1 i) x^ (+ x^ y)))])))

(define harvey
  (lambda (m n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 m)) (* 2 n)]
      [else (harvey (sub1 m) (harvey m (sub1 n)))])))

;; A single-argument version A(k) = A(k, k) that increases both m and n
;; at the same time dwarfs every primitive recursive function, including
;; very fast-growing functions such as the exponential function, the
;; factorial function, multi- and superfactorial functions, and even
;; functions defined using Knuth's up-arrow notation (except when the
;; indexed up-arrow is used). It can be seen that A(n) is roughly
;; comparable to fω(n) in the fast-growing hierarchy.
;; -- wiki

(define h
  (lambda (n)
    (harvey n n)))

;; Testing our implementations

(test-check "ack-peter 3,4"
  (ack-peter 3 4)
  125)

(test-check "sudan_1 5,6"
  (sudan 1 5 6)
  440)

(test-check "harvey 1 2"
  (harvey 1 2)
  4)

(test-check "h 1"
  (h 1)
  2)

(test-check "harvey 3 3 is quotidian"
  (harvey 3 3)
  16)

(test-check "h 3"
  (h 3)
  16)

(define count-digits
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (add1 (count-digits (quotient n 10)))))))

(test-check "harvey 3 5 is big"
  (count-digits (harvey 3 5))
  19730)
