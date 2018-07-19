(ns sicp.chapter1
  (:import (clojure.lang BigInt)))

(defn sqrt [x]
  (letfn [(average [x y] (/ (+ x y) 2))
          (improve [guess] (average guess (/ x guess)))
          (good-enough? [guess]
            (< (Math/abs (- guess (improve guess))) (* guess 0.0001)))
          (sqrt-itr [guess]
            (if (good-enough? guess)
              guess
              (sqrt-itr (improve guess))))]
    (sqrt-itr 1.0)))

(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1) (A x (- y 1)))))

(defn count-change [amount]
  (letfn [(first-denomination [kinds-of-coins]
            (cond (= kinds-of-coins 1) 1
                  (= kinds-of-coins 2) 5
                  (= kinds-of-coins 3) 10
                  (= kinds-of-coins 4) 25
                  (= kinds-of-coins 5) 50))
          (cc [amount kinds-of-coins]
            (cond (= amount 0) 1
                  (< amount 0) 0
                  (= kinds-of-coins 0) 0
                  :else (+ (cc amount (- kinds-of-coins 1))
                           (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))]
    (cc amount 5)))

;; Exercise 1.11
(defn f-recursive [n]
  (cond (< n 3) n
        :else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3))))))

(defn f-iter [n]
  (if (< n 3)
    n
    (letfn [(f-helper [a b c i]
              (if (= i n)
                c
                (f-helper b c (+ c (* 2 b) (* 3 a)) (+ 1 i))))]
      (f-helper 0 1 2 2))))

;; Exercise 1.12
;; Pascal's triangle
;; ........1.........
;; .......1 1........
;; ......1 2 1.......
;; .....1 3 3 1......
;; ....1 4 6 4 1.....
;; ...1 510 105 1....
;; ..1 6 1520156 1...
(defn pascal [row col]
  (if (or (= col 0) (= col row))
    1
    (+ (pascal (- row 1) (- col 1))
       (pascal (- row 1) col))))

(defn square [n]
  (* n n))

(defn fast-expt [b n]
  (cond
    (= n 0) 1
    (odd? n) (* b (fast-expt b (- n 1)))
    :else (square (fast-expt b (/ n 2)))))

;; Exercise 1.16
(defn fast-expt-2 [b n]
  (letfn [(expt-itr [b n a]
            (cond (= n 0) a
                  (odd? n) (expt-itr b (- n 1) (* a b))
                  :else (expt-itr (square b) (/ n 2) a)))]
    (expt-itr b n 1)))

;; Exercise 1.17
(defn add-mult [a b]
  (letfn [(double [x] (+ x x))
          (halve [x] (/ x 2))
          (* [a b]
            (cond (= a 1) b
                  (even? a) (* (halve a) (double b))
                  :else (+ b (* (- a 1) b))))]
    (* a b)))

(defn add-mult-2 [a b]
  (letfn [(double [x] (+ x x))
          (halve [x] (/ x 2))
          (* [a b c]
            (println "(* " a " " b " " c)
            (cond (= a 0) c
                  (even? a) (* (halve a) (double b) c)
                  :else (* (- a 1) b (+ c b))))]
    (* a b 0)))

(defn fib [n]
  (letfn [(fib-iter [a b p q count]
            (cond (= count 0) b
                  (even? count) (fib-iter a
                                          b
                                          (+ (* p p) (* q q))
                                          (+ (* 2 p q) (* q q))
                                          (/ count 2))
                  :else (fib-iter (+ (* b q) (* a q) (* a p))
                                  (+ (* b p) (* a q))
                                  p
                                  q
                                  (- count 1))))]
    (fib-iter 1 0 0 1 n)))

(defn smallest-divisor [n]
  (letfn [(divides? [n divisor] (= 0 (mod n divisor)))
          (find-divisor [n test-divisor]
            (cond (> (square test-divisor) n) n
                  (divides? n test-divisor) test-divisor
                  :else (find-divisor n (+ 1 test-divisor))))]
    (find-divisor n 2)))

(defn prime? [n]
  (= n (smallest-divisor n)))

;; Exercise 1.22
(defn find-three-smallest-primes-after [n]
  (letfn [(print-time [n]
            (time (prime? n))
            n)
          (helper [n count]
            (println n)
            (if (= count 0) nil
              (if (prime? n)
                (helper (+ 1 (print-time n)) (- count 1))
                (helper (+ 1 n) count))))]
    (helper (+ 1 n) 3)))

;; Exercise 1.23
(defn smallest-divisor [n]
  (letfn [(next [n] (if (= 2 n) 3 (+ n 2)))
          (divides? [n divisor] (= 0 (mod n divisor)))
          (find-divisor [n test-divisor]
            (cond (> (square test-divisor) n) n
                  (divides? n test-divisor) test-divisor
                  :else (find-divisor n (next test-divisor))))]
    (find-divisor n 2)))

;; Exercise 1.24
(defn bsquare [n]
  (*' n n))

(defn expmod [^BigInt base ^BigInt exp ^BigInt m]
  (cond (= exp 0) 1
        (even? exp) (mod (expmod (bsquare base) (/ exp 2) m) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (letfn [(try-it [a]
            (= (expmod (biginteger a) (biginteger n) (biginteger n)) a))]
    (try-it (+ 1 (rand-int (- n 1))))))

;; Exercise 1.27
(defn prime-test-full [n]
  (letfn [(helper [n i]
            (cond (= i n) true
                  :else
                  (if (not= (expmod (biginteger i) (biginteger n) (biginteger n)) i)
                    false
                    (helper n (+ 1 i)))))]
    (helper n 1)))

;; Exercise 1.29
(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn simpsons-integral [f a b n]
  (let [h (/ (- b a) n)
        add-kh (fn [x] (+ x h))
        add-2kh (fn [x] (+ x h h))]
    (/ (* h (+ (* 2 (sum f (+ a h) add-kh b))
               (* 2 (sum f (+ a h) add-2kh b))))
       3)))

;; Exercise 1.30
(defn sum-iter [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (next a) (+ result (term a)))))]
    (iter a 0)))

;; Exercise 1.31
(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(defn product-iter [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (next a) (* result (term a)))))]
    (iter a 1)))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi-1 [accuracy-n]
  (letfn [(term [i]
            (if (even? i)
              (/ (+ 2 i) (+ 1 i))
              (/ (+ 1 i) (+ 2 i))))]
    (* 4 (product-iter term 1 inc accuracy-n))))

;; Exercise 1.32
(defn accumulate-iter [combiner null-value term a next b]
  (letfn [(helper [a result]
            (if (> a b)
              result
              (helper (next a) (combiner result (term a)))))]
    (helper a null-value)))

(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(defn accum-sum [term a next b]
  (accumulate + 0 term a next b))

(defn accum-product [term a next b]
  (accumulate * 1 term a next b))

;; Exercise 1.33
