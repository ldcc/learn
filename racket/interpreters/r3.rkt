#lang racket

;;; structure

(struct closure (fun env))

(struct scope (tab sup))

;;; env

(define denv
  (map cons
       (list '+ '- '* '/)
       (list  +  -  *  /)))

(define lookup
  (λ (a env)
    (let ([v (assq a env)])
      (cond
        [(not v) (error (format "unbound variable ~a, env: ~a" a env))]
        [else (cdr v)]))))

(define ext-env
  (λ (a v env)
    (cons (cons a v) env)))

(define ext-env*
  (λ (a* v* env*)
    `(,@(map cons a* v*) ,@env*)))

;;; main code

(define interp
  (λ (exp env)
    (match exp
      [(? number?) exp]
      [(? symbol?)
       (lookup exp env)]
      [`(begin ,e* ... ,en)
       (for ([e e*])
         (interp e env))
       (interp en env)]
      [`(λ ,a ,e ...)
       (closure `(λ ,a (begin ,@e)) env)]
      [`(,f ,a ...)
       (let ([funv (interp f env)]
             [argv (map (λ (v) (interp v env)) a)])
         (match funv
           [(? procedure?)
            (apply funv argv)]
           [(closure `(λ ,a* ,e*) env-save)
            (interp e* (ext-env* a* argv env-save))]
           [_ (error "badmatch" funv)]))])))

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
