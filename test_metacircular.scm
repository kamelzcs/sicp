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

