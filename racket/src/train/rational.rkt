#lang racket

;; ----- rats -----
(define +rat
  (lambda (x y)
    (make-rat
     (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y)))))

(define -rat
  (lambda (x y)
    (make-rat
     (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y)))))

(define *rat
  (lambda (x y)
    (make-rat
     (* (numer x) (numer y))
     (* (denom x) (denom y)))))

(define /rat
  (lambda (x y)
    (make-rat
     (* (numer x) (denom y))
     (* (denom x) (numer y)))))


;;; ----- main code -----

;;  ----- constructor -----
(define make-rat
  (lambda (n d)
    (let ([g (gcd n d)])
      (cond [(and (number? n) (number? d))
             (cons (/ n g) (/ d g))]
            [else (error "not a number")]))))

;;  ----- selectors -----
(define numer
  (lambda (n)
    (car n)))

(define denom
  (lambda (n)
    (cdr n)))

;;  ----- interpreter -----
(define ratting
  (lambda (exp)
      (match exp
        [(? number? x) x]
        [`(,op ,e1 ,e2)
         (let ([l (ratting e1)]
               [r (ratting e2)]
               [o (match op
                    ['+ +rat]
                    ['- -rat]
                    ['* *rat]
                    ['/ /rat]
                    [(or 'cons 'list 'make-rat) make-rat])])
           (o l r))]
        [(? pair? x) x]
        [else (error "not a structure")])))

;;  ----- rat printer -----
(define rat
  (lambda (exp)
    (let ([c (ratting exp)])
      (let ([n (numer c)]
            [d (denom c)])
        (if (= d 1)
          [printf "~n ~a~n" (numer c)]
          [fprintf (current-output-port)
                   "~n ~a~n———~n ~a~n"
                   (numer c)
                   (denom c)])))))



;;  ----- examples -----
(define a (make-rat 1 4))
(define b (cons 2 4))

(rat `(/ (make-rat 2 3)
         (make-rat 1 3)))
;;  => 2

(rat `(/ ,a ,b))
;;  => 1/2

(rat `(+ (* ,a ,b) ,b))
;;  => 5/8