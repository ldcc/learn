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
        [v? v?]
        [else (error 'unbound-variable)]))))

(define ext-env
  (λ (a v env)
    (hash-set! env a v)
    env))

(define ext-env*
  (λ (a* v* env)
    (for ([a a*]
          [v v*])
      (hash-set! env a v))
    env))

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
       (let ([func (interp f env)]
             [arg* (map (λ (a) (interp a env)) a*)])
         (cond
           [(= (length a*) (length arg*))
            (match func
              [(? procedure?)
               (apply func arg*)]
              [(closure `(λ ,a* ,e*) env-save)
               (interp e* (ext-env* a* arg* env-save))]
              [_ (error 'expression-badmatch func)])]
           [else (error 'arity-mismatch)]))])))

;; user interface
(define r3
  (λ (exp)
    (interp `(begin ,@exp) denv)))

(define repl 
  (λ ()
    (display "REPL ⇒ ")
    (let ([exp (read)])
      (cond
        [(equal? exp '(exit)) "exit"]
        [else
         (printf "REPL ⇒ ~a~n" (r3 `(,exp)))
         (repl)]))))

;(r3
; '((define c 20)
;   (define a 10)
;   ((λ (x y o) (o x y)) a c +)))
;
;(r3
; '((+ 1 1)))

(repl)