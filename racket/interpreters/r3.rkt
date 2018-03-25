#lang racket

;;; structure

(struct closure (fun env))

;;; env

(define denv
  (make-hash
   (map cons
        '(+   -  *  /)
        `(,+ ,- ,* ,/))))

(define lookup
  (λ (a env)
    (let ([v? (hash-ref env a #f)])
      (cond
        [(not v?) (error (format "unbound variable"))]
        [else v?]))))

(define ext-env
  (λ (a v env)
    (hash-set! env a v)
    env))

(define ext-env*
  (λ (a* v* env*)
    (for ([a a*]
          [v v*])
      (hash-set! env* a v))
    env*))

;;; main code

(define interp
  (λ (exp env)
    (match exp
      [(? number?) exp]
      [(? symbol?)
       (lookup exp env)]
      [`(begin ,e* ... ,en)
       (for ([e e*]) (interp e env))
       (interp en env)]
      [`(define ,a ,e)
       (ext-env a (interp e env) env)]
      [`(λ ,a ,e ...)
       (closure `(λ ,a (begin ,@e)) env)]
      [`(,f ,a* ...)
       (let ([funv (interp f env)]
             [argv (map (λ (a) (interp a env)) a*)])
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

(r3
 '(begin
    (define c 20)
    (define a 10)
    (+ a c)))











