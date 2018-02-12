#lang racket

;;  ----- "Monte Carlo Method" for estimating PI: -----
;;  ----- Prob(gcd (r1 r2) = 1) ≈ 10 / (PI * PI) -----

(define (rand) (random 10))

(define estimate-pi
  (lambda (n)
    (letrec ([test (lambda (passed)
                     (cond [(= (gcd (rand) (rand)) 1)
                            (+ 1 passed)]
                           [else passed]))]
             [iter (lambda (passed remaining)
                     (cond [(<= remaining 0) (/ passed n)]
                           [else (iter (- remaining 1)
                                       (test passed))]))])
      (sqrt (/ 10 (iter 0 n))))))

;;  ----- test -----
(estimate-pi 10000)