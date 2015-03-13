;4.16
(define test (lambda (a b c)
               (define u 1)
               (+ a b c (+ u v))
               (define v 3)))

(test 1 2 3)

;4.17
(define test (lambda (a b c)
               (define u 1)
               (+ a b c (+ u v))
               (define v 3)))

(test 1 2 3)

;4.19
> (let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

a: undefined;
 cannot use before initialization

;4.20
(define (f x)
  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    (even? x)))

(f 10)

(letrec ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
  (fact 10))

;4.21

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 10)

;-->
((lambda (ft k)
   (if (= k 1)
     1
     (* k (ft ft (- k 1)))))
 (lambda (ft k)
   (if (= k 1)
     1
     (* k (ft ft (- k 1)))))
 10)

;-->
(if (= 10 1)
  1
  (* 10
     ((lambda (ft k)
        (if (= k 1)
          1
          (* k (ft ft (- k 1)))))
      (lambda (ft k)
        (if (= k 1)
          1
          (* k (ft ft (- k 1)))))
      9)))

;-->
(* 10
   ((lambda (ft k)
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))
    (lambda (ft k)
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))
    9)))

;4.22
(let ((a (+ 1 5))) (+ a 1))

;4.23
(lambda (env)
  ((lambda (env) 
     (proc1 env) 
     (proc2 env))
   env)
  (proc3 env))

;4.24
(eval '(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n))) the-global-environment)

(let ((t0 0) (t1 0))
  (define (loop i n)
    (eval '(factorial 1234) the-global-environment)
    (if (< i n)
      (loop (+ i 1) n)))
  (set! t0 (runtime))
  (loop 0 1)
  (set! t1 (runtime))
  (- t1 t0))

;2106789
;3302853

;4.25
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;4.27
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

;4.28
(define (g x) (+ x 1))
(define (f g x) (g x))

;4.29
(define (square x)
  (* x x))
(square (id 10))

(eval '(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n))) the-global-environment)

(let ((t0 0) (t1 0))
  (define (loop i n)
    (eval '(factorial 1234) the-global-environment)
    (if (< i n)
      (loop (+ i 1) n)))
  (set! t0 (runtime))
  (loop 0 1)
  (set! t1 (runtime))
  (- t1 t0))

;9.864471 second
;0.027408 second

;4.31
(define x 1)
(define (p1 (e lazy)) e x)
(p1 (set! x (cons x '(2))))

(define (p2 e) e x)
(p2 (set! x (cons x '(2))))

;4.2.3
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(list-ref integers 17)

(car '(a b c))

;4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 1 100)
