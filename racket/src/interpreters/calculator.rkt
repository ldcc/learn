#lang racket

;;  ----- eazy -----
(define interp
  (lambda (exp)
    (match exp
      [(? number?) exp]
      [`(lambda (,x) ,e) (lambda (x) e)]
      [`(let ([,x ,e1]) ,e2)
       (let ([x (interp e1)])
         e2)]
      [`(,e1 ,e2)
       ((interp e1)
        (interp e2))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1)]
             [v2 (interp e2)])
         (match op
         ['+ (+ e1 e2)]
         ['- (- e1 e2)]
         ['* (* e1 e2)]
         ['/ (/ e1 e2)]))])))

(define r2
  (lambda (exp)
    (interp exp)))











