#lang racket

;; ----- environment -----
(define env0
  (map cons
       (list '+ '- '* '/)
       (list  +  -  *  /)))

(define (ext-env x v env)
  (cons (cons x v) env))

(define (lookup x env)
  (let ([p (assq x env)])
    (cond
      [(not p) #f]
      [else (cdr p)])))

;; ----- data structures -----
(struct closure (fun env))

;; ----- main code -----
(define (e2 exp)
  (interp exp env0))

(define (interp exp env)
  (match exp
    ['(exit) exp]
    [(? number?) exp]
    [(? symbol?) (lookup exp env)]
    [`(,x) (interp x env)]
    [(list fun (list v) -> e ... end)
     (closure exp env)]
    [(list e1 e2)
     (match (interp e1 env)
       [(closure (list fun (list v) -> e ... end) env-save)
        (interp e (ext-env v (interp e2 env) env-save))])]
    [(list e1 op e2 ...)
     ((interp op env) (interp e1 env) (interp e2 env))]))

(define (read-loop)
  (let ([exp (read)])
    (cond
      [(equal? exp '|.|) '()]
      [else (cons exp (read-loop))])))

(define (repl)
  (display "REPL=>")
  (let ([result (e2 (read-loop))])
    (cond
      [(equal? result '(exit)) "exit"]
      [else
       (printf "REPL ⇒ ~a~n" result)
       (repl)])))

