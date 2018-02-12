#lang racket

(define atom?
  (lambda (v)
    (and (not (null? v))
         (not (pair? v)))))

(define pattern
  (lambda (op)
    (match op
      ['+ +]
      ['- -]
      ['* *]
      ['/ /])))

(define base
  (lambda (op)
    (match op
      ['+ 0]
      ['- 0]
      ['* 1]
      ['/ 1])))


;;  ----- main code -----
(define reduce
  (lambda (f l b)
    (cond [(null? l) b]
          [(atom? (car l))
           (reduce f
                   (cdr l)
                   (f b (car l)))]
          [(pair? (car l))
           (reduce f
                   (cdr l)
                   (f b (calc (car l))))])))

(define calc
  (lambda (exp)
    (letrec ([f (car exp)]
             [e (cdr exp)]
             [l (length e)]
             [go (lambda (b)
                   (reduce (pattern f)
                           (cdr e)
                           b))])
      (go (cond
            [(atom? (car e))
             (car e)]
            [(pair? (car e))
             (calc (car e))]
            [(= l 1)
             (base f)]
            [(= l 0)
             (error e "expected: at least 1 arguments")])))))


;;  ----- sample -----
(calc '(+ (- 1 2) (* 3 4 5)))
(+ (- 1 2) (* 3 4 5))
;;  => 59

(calc '(- 1 (* 2 3 4) 5 6))
(- 1 (* 2 3 4) 5 6)
;;  => -34

(calc '(* 1 2 (/ 3 4 5) 6))
(* 1 2 (/ 3 4 5) 6)
;;  => 1+4/5

(calc '(/ 1 (+ 2 3 4) 5 6))
(/ 1 (+ 2 3 4) 5 6)
;;  => 1/270

(calc '(+ (+ 1 2) 3 (* 4 5 6) 7))
(+ (+ 1 2) 3 (* 4 5 6) 7)
;;  => 133