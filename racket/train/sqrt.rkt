#lang racket

;; an sqrt calc
(define sqrt
  (lambda (square)
    (letrec ([tolerance 0.00001]
             [avg (lambda (a b) (/ (+ a b) 2))]
             [close-enou? (lambda (o n) (< (abs (- o n)) tolerance))]
             [f (lambda (guess) (/ (+ guess (/ square guess)) 2))]
             [iter (lambda (o n)
                     (cond [(close-enou? o n) n]
                           [else (iter n (f n))]))]
             [fixed-point (lambda(start) (iter start (f start)))])
      (fixed-point 1))))

;;; return something what depending by your usage
;;; for example:

;; return an procedure
(define average
  (lambda (f)
    (lambda (x)
      (/ (f x) 2))))

;; return an value
(define average1
  (lambda (f)
    (lambda (x)
      (/ (+ f x) 2))))

;; trying to use newton method for get sqrt
(define newton-sqrt
  (lambda(sq)
    (letrec ([dx 0.000000000001]
             [guess 1]
             [enou? (lambda (o n) (< (abs (- o n)) dx))]
             [iter (lambda (o n) (if (enou? o n) n (iter n (newton n))))]
             [newton (lambda (x) (/ (+ x (/ sq x)) 2))])
      (iter guess (newton guess)))))

;; trash
;             [square (lambda (x) (* x x))]
;             [f (lambda (x) (- sq (square x)))]
;             [df (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))]
;             [newton (lambda (guess) (- guess (/ (f guess) (df guess))))]