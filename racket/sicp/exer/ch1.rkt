#lang racket

;; ----- 1.1 -----
; skip


;; ----- 1.2 -----
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;; ----- 1.3 -----
(define (great-sum x y z)
  (cond ((< x y)
         (cond ((< x z) (+ y z))
               (else (+ x y))))
        ((< y z) (+ x z))
        (else (+ x y))))


;; ----- 1.4 -----
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; 当 b 大于 0 时，返回过程 <+>，否则返回过程 <->
; 然后用 a 和 b 调用这个过程，a + b ⇔ a - (-b)


;; ----- 1.5 -----
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; (test 0 (p))
; 解释器采用应用序求值：
; (p) 总是会被即时求值，最终会导致无限循环

; 解释器采用正则序求值：
; (p) 在 if 的 <alternative> 部分进行求值，
; 由于 x 的值为 0，所以最后会返回 0


;; ----- 1.6 -----
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; 由于 new-if 是一个基本过程，那么应用序求值会
; 导致在调用过程时所有参数被即时求值，由于在子句
; 部分使用了递归，程序将永远无法退出


;; ----- 1.7 -----
(sqrt 0.0003)
; 当这个数字很小时，其改进值虽然没有达到所要求的
; 准确度但却已经能够满足 good-enough? 过程

(sqrt 4294967296)
; 当这个数字很大时，其改进值在求平均值做加法时会
; 导致计算机的数值精度溢出

(define (improve-sqrt x)
  (improve-sqrt-iter 1.0 x))

(define (improve-good-enough? guess x)
  (< (abs (- (improve guess x)
             guess))
     0.001))

(define (improve-sqrt-iter guess x)
  (if (improve-good-enough? guess x)
      (improve guess x)
      (improve-sqrt-iter (improve guess x)
                         x)))

; 对于很小的数时可以工作，但在精度溢出时还是无法工作


;; ----- 1.8 -----
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (cbrt-ge? guess x)
      (improve-cbrt guess x)
      (cbrt-iter (improve-cbrt guess x)
                 x)))

(define (cbrt-ge? guess x)
  (< (abs (- (improve-cbrt guess x)
             guess))
     0.001))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))


;; ----- 1.9 -----
(define (sum1 a b)
  (if (= a 0)
      b
      (add1 (sum1 (sub1 a) b))))

(define (sum2 a b)
  (if (= a 0)
      b
      (sum2 (sub1 a) (add1 b))))

; sum1 的计算过程是线性递归
; sum2 的计算过程是线性迭代


;; ----- 1.10 -----
; the Ackermann function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
; f(n) = 2n

(define (g n) (A 1 n))
; g(n) = 2^n

(define (h n) (A 2 n))
; h(n) = 2^h(n-1)

;; ----- 1.11 -----
; linear recursive
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))

; linear iteration
(define (f2 n)
  (define (iter n a b c)
    (if (= n 0)
        (+ a b c)
        (iter (- n 1)
            (+ a b c)
            (* 2 a)
            (* 3 (/ b 2)))))
  (if (< n 3)
      n
      (iter (- n 3) 2 2 0)))

;; ----- 1.12 -----
; pascal's triangle
(define (pascal n)
  1)

;; ----- 1.15 -----
(define (cube x) (* x x x))

(define (p1 x)
  (println "x")
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p1 (sine (/ angle 3.0)))))

;a) 在求值 12.15 时，p 将被使用 5 次
;b) 过程 sine 所产生的计算过程使用的空间和步数
;   增长的阶都是 theta(log n)










