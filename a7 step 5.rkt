#lang racket
(require rackunit)
(require racket/trace)
;Problem 3
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,n) (apply-k k n))
      (`(mult ,x1 ,x2) (* (value-of-cps x1 env k) (value-of-cps x2 env k)))
      (`(sub1 ,x) (value-of-cps x env (lambda (v)
                                        (apply-k k (sub1 v)))))
      (`(zero ,x) (value-of-cps x env (lambda (v)
                                        (apply-k k (zero? v)))))
      (`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v)
                                                         (if v
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)))))
      (`(catch ,body) 
       (value-of-cps body (extend-env k env) k))
      (`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k^)
                                                        (value-of-cps v-exp env (lambda (v)
                                                                                  (apply-k k^ v))))))
      (`(let ,e ,body) (value-of-cps e env (lambda (a)
                                             (value-of-cps body (extend-env a env) k))))
      (`(var ,y) (apply-env env y k))
      (`(lambda ,body)
       (apply-k k (lambda (a k^)
                    (value-of-cps body (extend-env a env) k^))))
      (`(app ,rator ,rand) (value-of-cps rator env (lambda (f)
                                                     (value-of-cps rand env (lambda (w)
                                                                              (apply-closure f w k)))))))))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env
  (lambda (a env-cps)
    `(extend-env ,a ,env-cps)))


(define apply-env
  (λ (env y k)
    (match env
      (`(empty-env) (error 'val-of "unbound ~a" y))
      (`(extend-env ,a ,env)
       (cond
         ((zero? y) (apply-k k a))
         (else (apply-env env (sub1 y) k)))))))


(define apply-closure
  (λ (clos-cps arg k)
    (clos-cps arg k)))

(define apply-k
  (λ (k v)
    (k v)))

 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))



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