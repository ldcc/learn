#lang racket

;; ----- environment -----
(define env0 '())

(define ext-env
  (λ (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (λ (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))

;; ----- data structures -----
(struct Closure (fun env))

;; ----- main code -----
(define e2
  (lambda (exp)
    (interp exp env0)))

(define interp
  (λ (exp env)
    (match exp
      [(? symbol? v)
       (lookup v env)]
      [(? number? v) v]
      [(list fun (list v) -> e ... end)
       (Closure exp env)]
      [(list e1 e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure (list fun (list v) -> e ... end) env-save)
            (interp e (ext-env v v2 env-save))]))]
      [(list e1 op e2 ...)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))]
      [(list e) (interp e env)])))

(define repl
  (lambda ()
    (call/cc 
     (lambda (k)
       (display "REPL=>")
       (let ([exp (read)])
         (if (not (equal? '(exit) exp))
             (displayln (e2 exp))
             (k "exit")))
       (repl)))))