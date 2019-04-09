;; 1. (lambda (x y) (p (+ 8 x) (q y)))
(lambda (x y cont)
  (q/k y (lambda (val)
           (p/k (+ 8 x) val cont))))

;; 2. (lambda (x y u v) (+ 1 (f (g x y) (+ u v))))
(lambda (x y u v cont)
  (g/k x y (lambda (val1)
             (f/k val1 (+ u v) (lambda (val2)
                                 (cont (+ 1 val2)))))))

;; 3. (+ 1 (f (g x y) (+ u (h v))))
(h/k v (lambda (val1)
         (g/k x y (lambda (val2)
                    (f/k val2 (+ u val1) (lambda (val3)
                                           (cont (+ 1 val3))))))))

;; 4. (zero? (if a (p x) (p y)))
(if a
  (p/k x (lambda (val1)
           (cont (zero? val1))))
  (p/k y (lambda (val21)
           (cont (zero? val2)))))

;; 5. (zero? (if (f a) (p x) (p y)))
(f/k a (lambda (val1)
         (if val1
           (p/k x (lambda (val2)
                    (cont (zero? val2))))
           (p/k y (lambda (val2)
                    (cont (zero? val2)))))))

;; 6. (let ((x (let ((y 8)) (p y)))) x)
(let ((y 8))
  (p/k y (lambda (val1)
           (let ((x val1)) (cont x)))))

;; 7. (let ((x (if a (p x) (p y)))) x)
(if a
  (p/k x (lambda (val1)
           (let ((x val1)) (cont x))))
  (p/k x (lambda (val1)
           (let ((x val1)) (cont x)))))
