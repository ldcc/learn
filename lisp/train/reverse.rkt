#lang racket

;;  ----- get list size -----
(define size
  (lambda (l)
    (cond [(null? l) 0]
          [(pair? l) (+ (size (cdr l)) 1)])))

;;  ----- list getter -----
(define get
  (lambda (l i)
    (cond [(null? l) l]
          [(> i 0) (get (cdr l) (- i 1))]
          [(= i 0) (car l)])))

;;  ----- revert -----
(define revert-ori
  (lambda (l)
    (cond [(null? l) l]
          [else (cons (revert-ori (cdr l)) (car l))])))
;;  maybe work but another pl, look like this in lisp:
;;  => '((((((((() . 7) . 6) . 5) . 4) . 3) . 2) . 1) . 0)


;;  ----- old reverse (my first impl) -----
;;  ----- but (get) and (size) produce a huge of computation
(define reverse-old
  (lambda (l)
    (reversing-old l (- (size l) 1))))

(define reversing-old
  (lambda (l i)
    (cond [(< i 0) '()]
          [else (cons (get l i) (reversing-old l (- i 1)))])))

;;  ----- reverse v2
;;  ----- more easily, also more fast
(define reverse
  (lambda (l)
    (reversing l '())))

(define reversing
  (lambda (l1 l2)
    (cond [(null? l1) l2]
          [else (reversing (cdr l1) (cons (car l1) l2))])))


;;  ----- sample -----
(define l0 (list 0 1 2 3 4 5 6 7 8 9))
(define l1 (list 'a '123 'jhksdg 4 (list 5 6 7) 9))

(reverse-old l0)
;; => '(9 8 7 6 5 4 3 2 1 0)

(reverse-old l1)
;; => '(9 (5 6 7) 4 jhksdg 123 a)

(reverse l0)
;; => '(9 8 7 6 5 4 3 2 1 0)

(reverse l1)
;; => '(9 (5 6 7) 4 jhksdg 123 a)