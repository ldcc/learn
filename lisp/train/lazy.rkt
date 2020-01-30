#lang racket

(define (empty-stream? s)
  (eq? s '()))

(define empty-stream '())

(define (memo-proc e)
  (let ([to-run? #t])
    (λ ()
      (when to-run?
        (set! to-run? #f)
        (set! e (e)))
      e)))

(define-syntax-rule
  (cons-stream x y)
  (cons x (memo-proc (λ () y))))

(define (head s)
  (car s))

(define (tail s)
  ((cdr s)))

(define (map-stream proc s)
  (if (empty-stream? s)
      empty-stream
      (cons-stream (proc (head s))
                   (map-stream proc (tail s)))))

(define (range-stream i)
  (define (iter j)
    (print j)
    (if (= i j)
        empty-stream
        (cons-stream j (iter (+ j 1)))))
  (iter 0))

;; test tool
(define (1+ n) (+ n 1))

(define s (range-stream 4))
(display "\n")

(head (tail s))
(head (tail s))
(display "\n")

(head (tail (map-stream 1+ s)))
(head (tail (tail (map-stream 1+ s))))
(head (tail (tail (map-stream 1+ s))))