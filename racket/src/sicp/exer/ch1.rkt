#lang racket

;;  ----- exercise 1.1 -----
;;  lazy

;;  ----- exercise 1.2 -----
(define exp
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

;;  ----- exercise 1.3 -----
(define (suare x y)
  (+ (* x x) (* y y)))

(define (max x y)
  (cond [(> x y) x]
        [else y]))

(define (sum-square x y z)
  (cond [(or (> x y) (> x z))
         (suare x (max y z))]
        [else
         (suare y z)]))

;;  ----- exercise 1.5 -----
(define (p)
;;(println p)
  (p))

(define (test x y)
  (if (= x 0)
      0
;;    (println y)
      y))
;;  interpreter that uses applicative-order evaluation:
;;  (p) always evaluates first, final we got a infinite loop

;;  interpreter that uses normal-order evaluation:
;;  (q) never evalutes before (q) executing, the answer is 0

;;  ----- exercise 1.8 -----
