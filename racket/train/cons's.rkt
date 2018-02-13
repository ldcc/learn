#lang racket

;;  ----- it's kind of a data abstraction operation -----
;;  ----- also kind of a incredible power and magic -----
(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))

(define (cons2 x y)
  (lambda (z)
      (cond [(= z 'x) x]
            [(= z 'y) y])))

(define (car2 l)
  (l 'x))

(define (cdr2 l)
  (l 'y))


;;  ----- instance -----
(define a1 (cons1 1 2))
(define a2 (cons2 1 2))