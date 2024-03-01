#lang racket
(require rackunit)
(require racket/trace)
;Problem 3
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,n) (apply-k k n))
      
      (`(mult ,x1 ,x2) 
       (value-of-cps x1 env (make-mult-k-1 x2 env k)))
      
      (`(sub1 ,x) (value-of-cps x env (make-sub1-k x k)))
      
      (`(zero ,x) (value-of-cps x env (make-zero-k x k)))
      
      (`(if ,test ,conseq ,alt) (value-of-cps test env (make-if-k test conseq alt env k)))
      
      (`(catch ,body) 
       (value-of-cps body (extend-env k env) k))
      
      (`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env (make-pitch-k v-exp env)))
      
      (`(let ,e ,body) (value-of-cps e env (make-let-k body env k)))
      
      (`(var ,y) (apply-env env y k))
      
      (`(lambda ,body)
       (apply-k k (make-closure k body env)))
      
      (`(app ,rator ,rand) (value-of-cps rator env (make-app-k rand env k))))))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env
  (lambda (a env-cps)
    `(extend-env ,a ,env-cps)))

(define apply-env
  (lambda (env y k)
    (match env
      (`(empty-env) (error 'val-of "unbound ~a" y))
      (`(extend-env ,a ,env)
       (cond
         ((zero? y) (apply-k k a))
         (else (apply-env env (sub1 y) k)))))))


(define apply-closure
  (lambda (clos-cps a k)
    (match clos-cps
      (`(make-closure (λ (,x) ,body) ,env)
       (value-of-cps body (extend-env a env) k)))))

(define make-closure
  (lambda (x body env)
    `(make-closure (λ (,x) ,body) ,env)))

;Continuation Constructors
(define make-mult-k-1
  (lambda (x2^ env^ k^)
    `(make-mult-k-1 ,x2^ ,env^ ,k^)))

(define make-mult-k-2
  (lambda (v^ k^)
    `(make-mult-k-2 ,v^ ,k^)))

(define make-sub1-k
  (lambda(v^ k^)
    `(make-sub1-k ,v^ ,k^)))

(define make-zero-k
  (lambda (v^ k^)
    `(make-zero-k ,v^ ,k^)))

(define make-if-k
  (lambda (v^ conseq^ alt^ env^ k^)
    `(make-if-k ,v^ ,conseq^ ,alt^ ,env^ ,k^)))

(define make-pitch-k
  (lambda (v-exp^ env^)
    `(make-pitch-k ,v-exp^ ,env^)))

(define make-pitch-k^
  (lambda (k^^)
    `(make-pitch-k^ ,k^^)))

(define make-let-k
  (lambda (body^ env^ k^)
    `(make-let-k ,body^ ,env^ ,k^)))

(define make-app-k
  (lambda (rand^ env^ k^)
    `(make-app-k ,rand^ ,env^ ,k^)))

(define make-app-k-2
  (lambda (f^ k^)
    `(make-app-k-2 ,f^ ,k^)))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k) v]
      [`(make-mult-k-1 ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (make-mult-k-2 v k^))]
      [`(make-mult-k-2 ,v^ ,k^) (apply-k k^ (* v^ v))]
      [`(make-sub1-k ,v^ ,k^) (apply-k k^ (sub1 v))]
      [`(make-zero-k ,v^ ,k^) (apply-k k^ (zero? v))]
      [`(make-if-k ,v^ ,conseq^ ,alt^ ,env^ ,k^)
       (if v
          (value-of-cps conseq^ env^ k^)
          (value-of-cps alt^ env^ k^))]

      [`(make-pitch-k ,v-exp^ ,env^) (value-of-cps v-exp^ env^ (make-pitch-k^ v))]
      [`(make-pitch-k^ ,k^^) (apply-k k^^ v)]
      [`(make-let-k ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env v env^) k^)]
      [`(make-app-k ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (make-app-k-2 v k^))]
      [`(make-app-k-2 ,f^ ,k^) (apply-closure f^ v k^)])))

(define empty-k
  (lambda ()
    `(empty-k)))



;(trace apply-closure)
;(trace apply-env)
;Test Cases
(value-of-cps '(const 5) (empty-env) (empty-k))
(value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k))
(value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k))
(value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k))
(value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k))
(value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k))
(value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k))
(value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k))
(value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k))
(value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))
(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))
(value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k))
(value-of-cps '(catch (pitch (const 5) (pitch (var 0) (const 5)))) (empty-env) (empty-k))
(value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k))
(value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
              (empty-env)
              (empty-k))
(value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
              (empty-env)
              (empty-k))
(value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                    (lambda
                        (lambda
                            (if (zero (var 0))
                                (const 1)
                                (app (app (var 1) (var 1)) (sub1 (var 0)))))))
              (empty-env)
              (empty-k))