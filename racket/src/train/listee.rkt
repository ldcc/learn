#lang racket

;;  ----- list size -----
(define size
  (lambda (l)
    (letrec ([iter
              (lambda (l size)
                (cond [(null? l) size]
                      [(pair? l)
                       (iter (cdr l) (add1 size))]))])
      (iter l 0))))


;;  ----- getter/setter -----
(define et
  (lambda (l i)
    (lambda (op v)
      (cond
        [(>= i (size l))
         (error 'get/set
                "array index out of range, size: ~a, index: ~a"
                (size l) i)]
        [else (op l i v)]))))

(define get
  (lambda (l i)
    (letrec ([iter
              (lambda (l i v)
                (cond [(null? l) v]
                      [(> i v) (iter (cdr l) i (add1 v))]
                      [(= i v) (car l)]))])
      ((et l i) iter 0))))

(define set
  (lambda (l i v)
    (letrec ([iter
              (lambda (l i v)
                (cond [(null? l) l]
                      [else
                       (cons
                        (cond [(not (= i 0)) (car l)]
                              [else v])
                        (iter (cdr l) (sub1 1) v))]))])
      ((et l i) iter v))))


;;  ----- scaling -----
(define scale
  (lambda (l expe-s)
    (scaling l (size l) expe-s 0)))

(define scaling
  (lambda (l cs es ci)
    (cond [(< ci es)
           (cons (cond [(< ci cs) (get l ci)]
                       [else 0])
                 (scaling l cs es (add1 ci)))]
          [else '()])))



;;  ----- sample -----
(define l1 (list '(1 2) 3 '(4 5)))
(define l2 (list 1 "23e" 3 4 5 '()))
(define l3 (list 1 2 3 "kjsah" (+ 5 6)))
(define l4 (list (list 1 2) 3 4 5 (+ 6 7 8) 9))

;   ----- get size -----
((lambda (l)
   (size l))
 l1)
;;  => 3


;   ----- get member -----
;;  counting from 0
((lambda (l i)
   (get l i))
 l2 5)
;;  => '()

;;  will throw an error, cause last index is 5
;((lambda (l i)
;   (get l i))
; l2 6)
;;  => getter/setter: array index out of bound error, size: 6, index: 6


;   ----- set list member -----
;;  seems like set "kjsah" to 4, but a new list actually
;;  "set" will never change the origin
((lambda (l i v)
   (set l i v))
 l3 3 4)
;;  => '(1 2 3 4 11)


;   ----- expanding list scale -----
;;  0 as the value primary
((lambda (l s)
   (scale l s))
 l3 20)
;;  => '(1 2 3 "kjsah" 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)