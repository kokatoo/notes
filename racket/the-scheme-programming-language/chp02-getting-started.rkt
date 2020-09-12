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

;; Exercise 2.5.1
(let ([f (lambda (x) x)])
  (f 'a))
(let ([f (lambda x x)])
  (f 'a))
(let ([f (lambda (x . y) x)])
  (f 'a))
(let ([f (lambda (x . y) y)])
  (f 'a))

;; Exercise 2.5.2
(define list (lambda x x))

;; Exercise 2.5.3
;; no free variables
(lambda (f x)
  (f x))

;; +
(lambda (x)
  (+ x x))

;; f
(lambda (x y)
  (f x y))

;; cons, f, y
(lambda (x)
  (cons x (f x y)))

;; cons, y
(lambda (x)
  (let ([z (cons x y)])
    (x y z)))

;;cons, y, z
(lambda (x)
  (let ([y (cons x y)])
    (x y z)))

;; Exercise 2.6.1
(define doubler
  (lambda (f)
    (lambda (x) (f x x))))

(define double-any
  (lambda (f x)
    ((doubler f) x)))

(double-any double-any double-any)

;; Exercise 2.6.2
(define (compose p1 p2)
  (lambda (x)
    (p1 (p2 x))))

(define cadr (compose car cdr))
(define cddr (compose cdr cdr))

;; Exercise 2.6.3
(define caar (compose car car))
(define cadr (compose car cdr))

(define cdar (compose cdr car))
(define cddr (compose cdr cdr))

(define caaar (compose car caar))
(define caadr (compose car cadr))
(define cadar (compose car cdar))
(define caddr (compose car cddr))
(define cdaar (compose cdr caar))
(define cdadr (compose cdr cadr))
(define cddar (compose cdr cdar))
(define cdddr (compose cdr cddr))

(define caaaar (compose caar caar))
(define caaadr (compose caar cadr))
(define caadar (compose caar cdar))
(define caaddr (compose caar cddr))
(define cadaar (compose cadr caar))
(define cadadr (compose cadr cadr))
(define caddar (compose cadr cdar))
(define cadddr (compose cadr cddr))
(define cdaaar (compose cdar caar))
(define cdaadr (compose cdar cadr))
(define cdadar (compose cdar cdar))
(define cdaddr (compose cdar cddr))
(define cddaar (compose cddr caar))
(define cddadr (compose cddr cadr))
(define cdddar (compose cddr cdar))
(define cddddr (compose cddr cddr))

;; Exercise 2.7.1
(define (atom? x)
  (not (pair? x)))

;; Exercise 2.7.2
(define (shorter x y)
  (if (<= (length x) (length y))
      x
      y))

(shorter '(a b) '(c d e))
(shorter '(a b) '(c d))
(shorter '(a b) '(c))

;; Exercise 2.8.1
(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))

(define tree-copy-reverse
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy-reverse (cdr tr))
              (tree-copy-reverse (car tr))))))

(tree-copy '((a . b) . c))
(tree-copy-reverse '((a . b) . c))

;; Exercise 2.8.2
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append '(1 2 3) '(4 5 6))

;; Exercise 2.8.3
(define (make-list n obj)
  (if (= n 0)
      '()
      (cons obj (make-list (- n 1) obj))))
(make-list 7 '())

;; Exercise 2.8.4
(define (list-ref ls n)
  (if (= n 0)
      (car ls)
      (list-ref (cdr ls) (- n 1))))

(define (list-tail ls n)
  (if (= n 0)
      ls
      (list-tail (cdr ls) (- n 1))))

(list-ref '(1 2 3 4) 0)
(list-tail '(1 2 3 4) 0)
(list-ref '(a short (nested) list) 2)
(list-tail '(a short (nested) list) 2)

;; Exercise 2.8.5
(define (shorter x y)
  (define (shorter? ls1 ls2)
    (cond [(null? ls1) #t]
          [(null? ls2) #f]
          [else (shorter? (cdr ls1) (cdr ls2))]))

  (if (shorter? x y)
      x
      y))

(shorter '(a b) '(c d e))
(shorter '(a b) '(c d))
(shorter '(a b) '(c))

;; Exercise 2.8.6
(define (even? x)
  (cond [(= x 0) #t]
        [(= x 1) #f]
        [else (even? (- x 2))]))
(define (odd? x)
  (not (even? x)))

;; Exercise 2.8.7
(define (transpose lst)
  (cons (map car lst) (map cdr lst)))
(transpose '((a . 1) (b . 2) (c . 3)))

;; Exercise 2.9.1
(define (make-counter initial incr)
  (lambda ()
    (let ([next init])
      (lambda ()
        (let ([v next])
          (set! next (+ next incr))
          v)))))

;; Exercise 2.9.2
(define (make-stack)
  (let ([ls '()])
    (lambda (msg . args)
      (case msg
        [(empty? mt?) (null? ls)]
        [(push!) (set! ls (cons (car args) ls))]
        [(top) (car ls)]
        [(pop!) (set! ls (cdr ls))]
        [else "oops"]))))

;; Exercise 2.9.3
(define (make-stack)
  (let ([ls '()])
    (lambda (msg . args)
      (case msg
        [(empty? mt?) (null? ls)]
        [(push!) (set! ls (cons (car args) ls))]
        [(top) (car ls)]
        [(pop!) (set! ls (cdr ls))]
        [(ref) (list-ref ls (car args))]
        [(set!) (set-car! (list-tail ls (car args)) (cadr args))]
        [else "oops"]))))

;; Exercise 2.9.4
(define (make-stack n)
  (let ([v (make-vector n)]
        [i -1])
    (lambda (msg . args)
      (case msg
        [(empty? mt?) (= i -1)]
        [(push!)
         (set+ i (+ i 1))
         (vector-set! v i (car args))]
        [(top)
         (vector-ref v i)]
        [(pop!)
         (set! i (- i 1))]
        [(ref)
         (vector-ref v (- i (car args)))]
        [(set!)
         (vector-set! v (- i (car args)) (cadr args))]
        [else "oops"]))))

;; Exercise 2.9.5
(define (make-queue)
  (let ([end (mcons 'ignored '())])
    (mcons end end)))

(define (putq! q v)
  (let ([end (mcons 'ignored '())])
    (set-mcar! (mcdr q) v)
    (set-mcdr! (mcdr q) end)
    (set-mcdr! q end)))

(define (getq q)
  (mcar (mcar q)))

(define (delq! q)
  (set-mcar! q (mcdr (mcar q))))

(define (emptyq? q)
  (eq? (mcar q) (mcdr q)))

(define (getq q)
  (if (emptyq? q)
      (error 'getq "the queue is empty")
      (mcar (mcar q))))

(define (delq! q)
  (if (emptyq? q)
      (error 'delq! "the queue is empty")
      (set-mcar! q (mcdr (mcar q)))))

;; Exercise 2.9.6
(define (make-queue)
  (mcons '() '()))

(define (putq! q v)
  (let ([p (mcons v '())])
    (if (null? (mcar q))
        (begin (set-mcar! q p)
               (set-mcdr! q p))
        (begin (set-mcdr! (mcdr q) p)
               (set-mcdr! q p)))))

(define (getq q)
  (if (emptyq? q)
      (error 'getq "the queue is empty")
      (mcar (mcar q))))

(define (delq! q)
  (cond [(emptyq? q)
         (error 'getq "the queue is empty")]
        [(eq? (mcar q) (mcdr q))
         (begin (set-mcar! q '())
                (set-mcdr! q '()))]
        [else (set-mcar! q (mcdr (mcar q)))]))

;; Exercise 2.9.7
(define ls (mcons 'a '()))
(set-mcdr! ls ls)
(mpair? (mcdr ls))

(require compatibility/mlist)
(mlength ls)

;; Exercise 2.9.8
(define (race hare tortoise)
  (if (mpair? hare)
      (let ([hare (mcdr hare)])
        (if (mpair? hare)
            (and (not (eq? hare tortoise))
                 (race (mcdr hare) (mcdr tortoise)))
            (null? hare)))
      (null? hare)))

(define (list? x)
  (race x x))

(list? '())
(list? (list->mlist '(1 2 3)))
(list? '(a . b))
(list? (let ([ls (mcons 'a '())])
         (set-mcdr! ls ls)
         ls))
