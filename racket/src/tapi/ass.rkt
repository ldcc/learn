#lang racket

(define a1
  (lambda (c)
    (assoc c
           (list (list 1 2) (list 4 3))
           (lambda (a b)
             (= a b)))))

(define a2
  (lambda (c)
    (assoc c (list (list 1 2) (list 4 3)))))

(define a3
  (lambda (c)
    (assv c (list (list 1 2) (list 4 3)))))

(define a4
  (lambda (c)
    (assq c (list (list 1 2) (list 4 3)))))

(define a5
  (lambda (c)
    (assf (lambda (arg)
            (eq? arg c))
          (list (list 1 2) (list 4 3)))))