#lang racket

;; Exercise 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) (= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (square x) (* x x))
(define (larger x y) (if (> x y) x y))
(define (f x y z)
  (cond [(> x y) (+ (square x) (square (larger y z)))]
        [else (+ (square y) (square (larger x z)))]))
(f 1 2 3)

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
(test 0 (p))

;; 1.1.7 Example: Square Roots by Newton’s Method
(define (sqrt x)
  (define (square x) (* x x))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter 1.0 x))

(sqrt 9)

;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter-bad guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; Exercise 1.7
(define (good-enough-improved? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.000001))
(define (sqrt-iter-improved guess x)
  (if (good-enough-improved? guess (improve guess x))
      guess
      (sqrt-iter-improved (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter-improved 1.0 x))

;; Exercise 1.8
(define (cube x)
  (* x x x))
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-enough-improved? guess (improve-cube guess x))
      guess
      (cube-root-iter (improve-cube guess x) x)))
(define (cube-root x)
  (cube-root-iter 1.0 x))
(cube-root 8)

;; Lexical scoping
(define (sqrt x)

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))

;; 1.2.1 Linear Recursion and Iteration
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(require racket/trace)
(trace A)
(A 0 3)
(A 1 3)
(A 2 3)

;; 1.2.2 Tree Recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(fib 100)

;; Example: Counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(require racket/trace)
(trace cc)
(count-change 10)

;; Exercise 1.11
(define (f n)
  (cond [(< n 3) n]
        [else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3))))]))

(define (f n)
  (define (f-iter a b c count)
    (cond [(< n 3) n]
          [(> count n) a]
          [else (f-iter (+ a (* 2 b) (* 3 c)) a b (+ count 1))]))

  (f-iter 2 1 0 3))

(f 2)

;; Exercise 1.12
(define (pascal r c)
  (cond [(or (= c 1) (= c r)) 1]
        [else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))]))
(pascal 1 1)
(pascal 3 2)
(pascal 5 3)

;; Exercise 1.15
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

(untrace p)

;; 1.2.4 Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)

  (define (expt-iter acc i)
    (if (= i 0)
        acc
        (expt-iter (* acc b) (- i 1))))

  (expt-iter 1 n))

(define (fast-expt b n)
  (display n)
  (newline)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; Exercise 1.16
(define (fast-expt b n)

  (define (fast-expt-iter acc b n)
    (cond [(= n 0) acc]
          [(even? n) (fast-expt-iter acc (square b) (/ n 2))]
          [else (fast-expt-iter (* b acc) b (- n 1))]))

  (fast-expt-iter 1 b n))
(fast-expt 2 3)

;; Exercise 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond [(= b 0) 0]
        [(even? b) (* (double a) (halve b))]
        [else (+ a (* a (- b 1)))]))

;; Exercise 1.18
(define (* a b)
  (define (multi-iter acc a b)
    (cond [(= b 0) acc]
          [(even? b) (multi-iter acc (double a) (halve b))]
          [else (multi-iter (+ acc a) a (- b 1))]))

  (multi-iter 0 a b))

(* 120 2)

;; Exercise 1.1

(define (fib n)

  (define (fib-iter a b p q i)
    (cond [(= i 0) b]
          [(even? i)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 q p) (* q q))
                     (/ i 2))]
          [else
           (fib-iter (+ (* b q)
                        (* a q)
                        (* a p))
                     (+ (* b p)
                        (* a q))
                     p
                     q
                     (- i 1))]))

  (fib-iter 1 0 0 1 n))

;; 1.2.5 Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(require racket/trace)
(untrace gcd)

(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)

;; 1.2.6 Example: Testing for Primality
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; The Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))
(fermat-test 198)

;; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22
(define (runtime) (current-inexact-milliseconds))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin
        (report-prime n (- (runtime) start-time))
        #t)
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(timed-prime-test 7)

(define (search-for-primes start-range [i 0])
  (cond [(= i 3)
         (newline)
         (display "done")]
        [(even? start-range)
         (search-for-primes (+ 1 start-range) i)]
        [else
         (if (timed-prime-test start-range)
             (search-for-primes (+ 2 start-range) (+ i 1))
             (search-for-primes (+ 2 start-range) i))]))

(search-for-primes 100000000)
(search-for-primes 1000000000)
(search-for-primes 10000000000 0)

;; Exercise 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes 100000000)
(search-for-primes 1000000000)
(search-for-primes 10000000000)
