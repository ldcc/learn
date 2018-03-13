#lang racket

;;; structure

(struct closure {fun env})

(struct scope {tab sup})

;;; env

(define denv
  (map cons
       (list '+ '- '* '/)
       (list  +  -  *  /)))

(define lookup
  (λ (s env)
    (let ([v (assq s env)])
      (cond
        [(not v) (error "unbound variable" )]
        [else (cdr v)]))))

(define ext-env
  (λ (s v env)
    (cons (cons s v) env)))

;;; main code

(define interp
  (λ (exp env)
    (match exp
      [(? number?) exp]
      [(? symbol?)
       (lookup exp env)]
      [()]
      [(list e1 e2 ...)
       (let ([func (interp e1 env)]
             [vars (map (λ (x) (interp x env)) e2)])
         (match func
           [(? procedure?) (apply func vars)]
           [_ (error "badmatch" func)]))])))

;; user interface

(define r3
  (λ (exp)
    (interp exp denv)))

(define repl 
  (λ ()
    (display "REPL ⇒ ")
    (let ([exp (read)])
      (cond
        [(equal? exp '(exit)) "exit"]
        [else
         (printf "REPL ⇒ ~a~n" (r3 exp))
         (repl)]))))

