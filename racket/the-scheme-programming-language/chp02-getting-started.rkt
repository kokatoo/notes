#lang racket

;; Exercise 2.2.1
(+ (* 1.2 (- 2 1/3)) -8.7)
(/ (+ 2/3 4/9) (- 5/11 4/3))
(+ 1 (/ 1 (/ (+ 2 (/ 1 (+ 1 1/2))))))
(* 1 -2 3 -4 5 -6 7)

;; Exercise 2.2.3
(cons 'car 'cdr)
(list 'this '(is silly))
(cons 'is '(this silly?))
(quote (+ 2 3))
(cons '+ '(2 3))
(car '(+ 2 3))
(cdr '(+ 2 3))
cons (quote cons)
(quote (quote cons))
(car (quote (quote cons)))
(+ 2 3)
(+ '2 '3)
(+ (car '(2 3))
   (car (cdr '(2 3))))
((car (list + - * /)) 2 3)

;; Exercise 2.24
(car (cdr (car '((a b) (c d)))))
(car (car (cdr '((a b) (c d)))))
(car (cdr (car (cdr '((a b) (c d))))))

;; Exercise 2.25
'((a .b) ((c) d) ())

;; Exercise 2.26
(cons 1 (cons '(2 . ((3) . ())) (cons '(()) (cons 4 5))))

;; Exercise 2.4.1
(+ (- (* 3 a) b) (+ (* 3 a) b))
(let ([x (* 3 a)])
  (+ (- x b) (+ x b)))

(cons (car (list a b c)) (cdr (list a b c)))
(let ([x (list a bc)])
  (cons (car x) (cdr x)))

;; Exercise 2.4.2
(let ([x 9])
  (display x)
  (newline)
  (* x
     (let ([x (/ x 3)])
       (display x)
       (newline)
       (+ x x))))

;; Exercise 2.4.3
(let ([x 'a] [y 'b])
  (list (let ([x 'c]) (cons x y))
        (let ([y 'd]) (cons x y))))

(let ([x0 'a] [y0 'b])
  (list (let ([x1 'c]) (cons x1 y0))
        (let ([y1 'd]) (cons x0 y1))))

(let ([x '((a b) c)])
  (cons (let ([x (cdr x)])
          (car x))
        (let ([x (car x)])
          (cons (let ([x (cdr x)])
                  (car x))
                (cons
                 (let ([x (car x)]) x)
                 (cdr x))))))

(let ([x0 '((a b) c)])
  (cons (let ([x1 (cdr x0)])
          (car x1))
        (let ([x2 (car x0)])
          (cons
           (let ([x3 (cdr x2)]) (car x3))
           (cons (let ([x4 (car x2)]) x4)
                 (cdr x2))))))
