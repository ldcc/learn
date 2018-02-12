#lang racket

(define (suare x y)
  (+ (* x x) (* y y)))

(sin 45)
(tan 45)
(cos 45)
(asin 45)
(atan 45)
(acos 45)

(define (v1 degree)
  (suare (sin degree) (cos degree)))