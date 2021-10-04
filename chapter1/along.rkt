#lang sicp

;;;1.1

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;;Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* (- 6 2) (- 2 7)))

;;Exercise 1.3
(define (sum-of-larger-nums x y z)
  (cond ((and (>= y x) (>= z x)) (sum-of-squares y z))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;Exercise 1.7
(define (sqrt-iter-2 prev-guess guess x)
  (if (good-enough-2? prev-guess guess)
      guess
      (sqrt-iter-2 guess (improve-guess guess x) x)))

(define (good-enough-2? guess-1 guess-2)
  (<= (/ (abs (- guess-1 guess-2)) guess-2) 0.000001))

(define (sqrt2 x)
  (sqrt-iter-2 1.0 2.0 x))

;;Exercise 1.8
(define (cubert-iter prev-guess guess x)
  (if (good-enough-2? prev-guess guess)
      guess
      (cubert-iter guess (improve-cubert-guess guess x) x)))

(define (improve-cubert-guess guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))

(define (cubert x)
  (cubert-iter 1.0 2.0 x))

;;;1.2
(define (factorial-recursive n)
  (if (= 1 n)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;Exercise 1.9
; First
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

;Second
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9

;;Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (define (f n) (A 0 n)) => 2n
; (define (g n) (A 1 n)) => 2^n
    ; (g 4)
    ; (A 1 4)
    ; (A 0 (A 1 3))
    ; (A 0 (A 0 (A 1 2)))
    ; (A 0 (A 0 (A 0 (A 1 1))))
    ; (A 0 (A 0 (A 0 2)))
    ; (A 0 (A 0 4))
    ; (A 0 8)
    ; 16
; (define (h n) (A 2 n)) =>
    ; (h 4)
    ; (A 2 4)
    ; (A 1 (A 2 3))
    ; (A 1 (A 1 (A 2 2)))
    ; (A 1 (A 1 (A 1 (A 2 1))))
    ; (A 1 (A 1 (A 1 2)))
    ; (A 1 (A 1 (A 0 (A 1 1))))
    ; (A 1 (A 1 (A 0 2)))
    ; (A 1 (A 1 4))
    ; (A 1 (A 0 (A 1 3)))
    ; (A 1 (A 0 (A 0 (A 1 2))))
    ; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
    ; (A 1 (A 0 (A 0 (A 0 2))))
    ; (A 1 (A 0 (A 0 4)))
    ; (A 1 (A 0 8))
    ; (A 1 16)
    ; 2^16


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

;;Exercise 1.11
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (= count n)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1))))
  (if (< n 3)
      n
      (iter 0 1 2 2)))

;;Exercise 1.12
(define (pascal row col)
  (cond ((= 0 col) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))


;;Exercise 1.14
;                                         (count-change 11)
;                                            (cc 11 5)
;                                            /        \
;                                   (cc 11 4)          (cc -39 5)
;                                   /        \
;                           (cc 11 3)        (cc -14 4)
;                           /         \_____________________________________
;                  (cc 11 2)                                             (cc 1 3)
;                  /        \                                           /        \
;          (cc 11 1)    (cc 6 2)                                     (cc 1 2)   (cc -9 3)
;         /     \        /      \__________                         /     \
; (cc 11 0) (cc 10 1)   (cc 6 1)         (cc 1 2)            (cc 1 1) (cc 1 -3)
;          /  |         /      \          |     \             |      \
; (cc 10 0) (cc 9 1) (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2)   (cc 1 0) (cc 0 1) = 1
;          /   |             /   |          |  \______
;   (cc 9 0) (cc 8 1)   (cc 5 0) (cc 4 1) (cc 1 0)  (cc 0 1) = 1
;            /      \            /      \
;       (cc 8 0)   (cc 7 1)  (cc 4 0)   (cc 3 1)
;                  (cc 6 1)             (cc 2 1)
;                  (cc 5 1)             (cc 1 1)
;                  (cc 4 1)             (cc 0 1)
;                  (cc 3 1)
;                  (cc 2 1)
;                  (cc 1 1)
;                  (cc 0 1)
;
; Space complexity is proportional to depth of tree as we need to remember the nodes above only. O(n).
; Number of nodes for (cc n 1) = 2n + 1 ~ O(n)
; Number of nodes for (cc n 2) = (n/5)*2n + 1 ~ O(n^2)
; ... Number of nodes for (cc n 5) ~ O(n^5)
; Time complexity = O(n^5)

;; Exercise 1.15
; a. `sine` procedure will terminate when angle less than 0.1. So, procedure p is applied ceil (log 3 (12/0.1)) = 5 times.
; b. Space and number of steps growth is O(log(a))

(define (expt b n)
  (define (expt-iter prod counter)
    (if (= counter 0)
        prod
        (expt-iter (* prod b) (- counter 1))))
  (expt-iter 1 n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recursive b (/ n 2))))
        (else (* b (fast-expt-recursive b (- n 1))))))

;; Exercise 1.16
(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

;; Exercise 1.17
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
    (cond ((= b 0) 0)
          ((even? b) (mult (double a) (halve b)))
          (else (+ a (mult a (- b 1))))))

;; Exercise 1.18
(define (mult-iter a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))
  (iter a b 0))

;; Exercise 1.19
(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter a b (+ (square p) (square q)) (+ (square q) (* 2 p q)) (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))))
  (fib-iter 1 0 0 1 n))

;; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Prime test
(define (smallest-divisor n)
  (find-divisor n 2))

; from exercise 1.23
(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? n d)
  (= (remainder n d) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (remainder (- n 1) 4294967087)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (iter n)
    (cond ((< n end) (timed-prime-test n) (iter (+ n 2)))))
  (iter (if (even? start) (+ 1 start) start)))

; We use bigger numbers than suggested in book
; (search-for-primes 10000000 10000200)
; 10000019 *** 113
; 10000079 *** 127
; 10000103 *** 111
; avg 117
;
; (search-for-primes 100000000 100000090)
; 100000007 *** 395
; 100000037 *** 359
; 100000039 *** 332
; avg 362
;
; (search-for-primes 1000000000 1000000090)
; 1000000007 *** 1553
; 1000000009 *** 1061
; 1000000021 *** 1285
; avg 1299.66
; 
; (search-for-primes 10000000000 10000000090)
; 10000000019 *** 3713
; 10000000033 *** 4632
; 10000000061 *** 3858
; avg 4064.66
;
; (search-for-primes 100000000000 100000000090)
; 100000000003 *** 12123
; 100000000019 *** 12386
; 100000000057 *** 12167
; avg 12225.33
;

; Data does indeed show that the order of growth of time of prime? func is O(sqrt(2))

; (With modification to use next)

;; Exercise 1.23
; (search-for-primes 10000000 10000200)
; 10000019 *** 91
; 10000079 *** 76
; 10000103 *** 75
; avg 80.66
;
; (search-for-primes 100000000 100000090)
; 100000007 *** 367
; 100000037 *** 280
; 100000039 *** 261
; avg 302.66
;
; (search-for-primes 1000000000 1000000090)
; 1000000007 *** 860
; 1000000009 *** 776
; 1000000021 *** 729
; avg 788.33
;
; (search-for-primes 10000000000 10000000090)
; 10000000019 *** 2838
; 10000000033 *** 3577
; 10000000061 *** 2479
; avg 2964.66
;
; (search-for-primes 100000000000 100000000090)
; 100000000003 *** 8572
; 100000000019 *** 9430
; 100000000057 *** 8915
; avg 6827.0
;

; With fermat test
; (search-for-primes 10000000 10000200)
; 10000019 *** 26
; 10000079 *** 25
; 10000103 *** 25
; avg 25.33
;
; (search-for-primes 100000000 100000090)
; 100000007 *** 27
; 100000037 *** 28
; 100000039 *** 28
; avg 27.66
;
; (search-for-primes 1000000000 1000000090)
; 1000000007 *** 31
; 1000000009 *** 29
; 1000000021 *** 30
; avg 29
;
; (search-for-primes 10000000000 10000000090)
; 10000000019 *** 58
; 10000000033 *** 58
; 10000000061 *** 63
; avg 59.66
;
; (search-for-primes 100000000000 100000000090)
; 100000000003 *** 70
; 100000000019 *** 70
; 100000000057 *** 75
; avg 71.66
;
; (search-for-primes 1000000000000 1000000000090)
; 1000000000039 *** 72
; 1000000000061 *** 73
; 1000000000063 *** 75
; avg 73.33
;
; (search-for-primes 10000000000000 10000000000100)
; 10000000000037 *** 78
; 10000000000051 *** 79
; 10000000000099 *** 79
; avg 78.66

;; Exercise 1.25
; fast-expt generates large numbers while doing exponentiation while expmod intermediate results are always less than m. This could cause perf issues.

;; Exercise 1.26
; (expmod 5 9 9)
; (* 5 (expmod 5 8 9))
; (* 5 (* (expmod 5 4 9) (expmod 5 4 9)))
; (* 5 (* (* (expmod 5 2 9) (expmod 5 2 9)) (* (expmod 5 2 9) (expmod 5 2 9)))))
; (* 5 (* (* (* (expmod 5 1 9) (expmod 5 1 9)) (* (expmod 5 1 9) (expmod 5 1 9))) (* (* (expmod 5 1 9) (expmod 5 1 9)) (* (expmod 5 1 9) (expmod 5 1 9)))))))
; Depth of tree = log(n) (n being exponent) - number of steps exponential to tree depth. So, liner time. O(n)
; Or, consider (square (expmod base n m)) and (* (expmod base n m) (expmod base n m))
; Assume (expmod base n m) takes x steps.
; Double exponent. i.e . 2n.
; (square (expmod base 2n m)) => (square (square (expmod base n m))) => x + 1 steps = O(log(n)) growth
; (* (expmod base 2n m) (expmod base 2n m)) => (* (* (expmod base n m) (expmod base n m)) (* (expmod base n m) (expmod base n m)))) => 2x steps. O(n) growth

;; Exercise 1.27
(define (carmichael-test n)
  (define (iter x)
    (cond ((= x n) true)
          ((not (= (expmod x n n) x)) false)
          (else (iter (+ x 1)))))
  (iter 1))
        
;; Exercise 1.28
;; Cheated with: https://stackoverflow.com/questions/55969284/sicp-exercise-1-28-miller-rabin-at-least-half-the-numbers-will-reveal-a-non/59834347#59834347
(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check (miller-rabin-expmod base (/ exp 2) m)))
        (else (remainder (* base (miller-rabin-expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a n n) a))
  (try-it (+ 1 (random (remainder (- n 1) 4294967087)))))

(define (miller-rabin-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-fast-prime? n (- times 1)))
        (else false)))

;;; 1.3

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ 1 n))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (integral-next x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) integral-next b) dx))

;; Exercise 1.29
(define (simpson-integral f a b n)
  (define (simpson-term i)
    (* (cond ((or (= i 0) (= i n)) 1)
             ((even? i) 2)
             (else 4))
    (f (+ a (* i (/ (- b a) n))))))
  (/ (* (-  b a) (sum simpson-term 0 inc n)) (* 3 n)))

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n)
  (define (pi-term i)
    (/ (* i (+ i 2)) (square (+ i 1))))
  (define (pi-next i)
    (+ i 2))
  (* 4.0 (product pi-term 2 pi-next (* 2 n))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; Exercise 1.32
(define (accumulator combiner null-value term a next b)
  (define (acc term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (acc term (next a) next b))))
  (acc term a next b))

(define (acc-sum term a next b)
  (accumulator + 0 term a next b))

(define (acc-prod term a next b)
  (accumulator * 1 term a next b))

(define (accumulator-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (acc-sum-iter term a next b)
  (accumulator-iter + 0 term a next b))

;; Exercise 1.33
(define (filtered-accumulate filterer combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (if (filterer a) (term a) null-value) result))))
  (iter a null-value))

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(define (prod-of-relative-primes n)
  (define (coprime? i)
    (= (gcd i n) 1))
  (filtered-accumulate coprime? * 1 identity 1 inc n))

