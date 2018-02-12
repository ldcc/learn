#lang racket

;;  ----- base operate -----
(define atom?
  (lambda (v)
    (and (not (null? v))
         (not (pair? v)))))

(define map
  (lambda (f l)
    (cond
      [(null? l) l]
      [else
       (cons (f (car l))
             (map f (cdr l)))])))


(define pattern
  (lambda (exp)
    (match exp
      ['+ +]
      ['- -]
      ['* *]
      ['/ /]
      ['simplify simplify]
      [(? pair?) (simplify exp)]
      [else exp])))

;;  ----- simplify -----
(define simplify
  (lambda (exp)
    (caling (car exp) (cdr exp))))

(define caling
  (lambda (op exp)
    (match exp
      [`(,e1 ,e2)
       (let ([v1 (fs op e1)]
             [v2 (fs op e2)])
         ((pattern op) v1 v2))]
      [`(,e1 . ,e2)
       (let ([v1 (fs op e1)]
             [v2 (fs op e2)])
         ((pattern op) v1 v2))])))

(define fs
  (lambda (op e)
    (cond
      [(null? e) 0]
      [(atom? e) e]
      [(symbol? (car e))
       (simplify e)]
      [else
       (caling op e)])))

;;  ----- make list -----
(define clist
  (lambda (exp)
    (map simplify exp)))
