#lang racket

;; The following is a dramatization of the course of events of today's
;; class. Actual events may have slightly differed.

;; We defined plus as follows

;; (define plus
;;   (λ (x y)
;;     (cond
;;       ((zero? y) x)
;;       (else (add1 (plus x (sub1 y)))))))


;; We defined times as follows

;; (define times
;;   (λ (x y)
;;     (cond
;;       ((zero? y) 0)
;;       (else (plus x (times x (sub1 y)))))))


;; We then defined expo as follows

;; (define expo
;;   (λ (x y)
;;     (cond
;;       ((zero? y) 1)
;;       (else (times x (expo x (sub1 y)))))))

;; We noticed some mighty strange similarities. They look almost all
;; look almost like:

;; (define <name>
;;   (λ (x y)
;;     (cond
;;       ((zero? y) <base>)
;;       (else (<previous-function> x (<name> x (sub1 y)))))))

;; We abstracted from the operations we built last class, for this operation.

;; (define G
;;   (λ (i)
;;     (cond
;;       [(zero? i) (λ (x y)
;;                    (cond
;;                      [(zero? y) x]
;;                      [else (add1 ((G i) x (sub1 y)))]))]
;;       [(zero? (sub1 i)) (λ (x y)
;;                           (cond
;;                             [(zero? y) 0]
;;                             [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))]
;;       [else (λ (x y)
;;               (cond
;;                 [(zero? y) 1]
;;                 [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))])))

;; We "inverse staged" the code, by which we mean that we used a
;; single (λ (x y) ...) around the whole cond, instead of 3 different
;; (λ (x y) ...)s in each cond line.

;; (define G
;;   (λ (i)
;;     (λ (x y)
;;       (cond
;;         [(zero? i)
;;          (cond
;;            [(zero? y) x]
;;            [else (add1 ((G i) x (sub1 y)))])]
;;         [(zero? (sub1 i))
;;          (cond
;;            [(zero? y) 0]
;;            [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]
;;         [else
;;          (cond
;;            [(zero? y) 1]
;;            [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; We then realized that we could reverse the order of those
;; tests. Handle all the cases where y is zero at once, and then handle
;; all the cases where y is non-zero.

;; (define G
;;   (λ (i)
;;     (λ (x y)
;;       (cond
;;         [(zero? y) 
;;          (cond
;;            [(zero? i)  x]
;;            [(zero? (sub1 i)) 0]
;;            [else 1])]
;;         [else 
;;          (cond
;;            [(zero? i) (add1 ((G i) x (sub1 y)))]
;;            [(zero? (sub1 i)) ((G (sub1 i)) x ((G i) x (sub1 y)))]
;;            [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; We found a duplicated line, so we removed it. 

;; (define G
;;   (λ (i)
;;     (λ (x y)
;;       (cond
;;         [(zero? y) 
;;          (cond
;;            [(zero? i)  x]
;;            [(zero? (sub1 i)) 0]
;;            [else 1])]
;;         [else
;;           (cond
;;             [(zero? i) (add1 ((G i) x (sub1 y)))]
;;             [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; That [else (cond ...) ...] was a little verbose for our tastes, so we
;; got rid of it.

;; (define G
;;   (λ (i)
;;     (λ (x y)
;;       (cond
;;         [(zero? y) 
;;          (cond
;;            [(zero? i)  x]
;;            [(zero? (sub1 i)) 0]
;;            [else 1])]
;;         [(zero? i) (add1 ((G i) x (sub1 y)))]
;;         [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))))

;; We can get rid of that nested cond by in-lining the (zero? y) test.

;; (define G
;;   (λ (i)
;;     (λ (x y)
;;       (cond
;;         [(and (zero? y) (zero? i))  x]
;;         [(and (zero? y) (zero? (sub1 i))) 0]
;;         [(zero? y) 1]
;;         [(zero? i) (add1 ((G i) x (sub1 y)))]
;;         [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))))

;; And we really have no need to have nested those function calls, so
;; let's de-nest them.

(define G
  (λ (i x y)
    (cond
      [(and (zero? y) (zero? i)) x]
      [(and (zero? y) (zero? (sub1 i))) 0]
      [(zero? y) 1]
      [(zero? i) (add1 (G i x (sub1 y)))]
      [else (G (sub1 i) x (G i x (sub1 y)))])))

