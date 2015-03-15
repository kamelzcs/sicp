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

(define (a-pythagorean-triple-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-from 1)

;4.39
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))(require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
 (let ((cooper (amb 2 3 4 5))
       (miller (amb 3 4 5)))
   (require (> miller cooper))
   (let ((fletcher (amb 2 3 4)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (let ((smith (amb 1 2 3 4 5)))
       (require (not (= (abs (- smith fletcher)) 1)))
       (let ((baker (amb 1 2 3 4)))
         (require
           (distinct? (list baker cooper fletcher miller smith)))
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith)))))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling) 
  (let ((baker (amb 1 2 3 4))  
        (cooper (amb 2 3 4 5))  
        (fletcher (amb 2 3 4))) 
    (require (not (= (abs (- fletcher cooper)) 1))) 
    (let ((miller (an-integer-between (+ 1 cooper) 5))  
          (smith (amb 
                   (an-integer-between 1 (- fletcher 2)) 
                   (an-integer-between (+ fletcher 2) 5)))) 
      (require 
        (distinct? (list baker cooper fletcher miller smith))) 
      (list 
        (list 'baker baker) 
        (list 'cooper cooper) 
        (list 'fletcher fletcher) 
        (list 'miller miller) 
        (list 'smith smith))))) 

(multiple-dwelling)

;4.41

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (permutations lst)
  (if (null? lst)
    (list '())
    (flatmap
      (lambda (first)
        (map
          (lambda (rest) (cons first rest))
          (permutations (filter (lambda (x) (not (= x first))) lst))))
      lst)))

(permutations '(1 2 3 4 5))

;4.42

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (xor a b)
  (or (and a (not b))
      (and (not a)b)))

(define liars
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
      (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
