#lang racket
(require rackunit)

;Problem 3
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,n) (apply-k k n))
      (`(mult ,x1 ,x2) (* (value-of-cps x1 env k) (value-of-cps x2 env k)))
      (`(sub1 ,x) (value-of-cps x env (lambda (v)
                                        (sub1 v))))
      (`(zero ,x) (zero? (value-of-cps x env k)))
      (`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v)
                                                         (if v
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)))))
      (`(catch ,body) 
       (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ k) (apply-env env (sub1 y) k^))) k))
      (`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k^)
                                                        (value-of-cps v-exp env (lambda (v)
                                                                                  (apply-k k^ v))))))
      (`(let ,e ,body) (value-of-cps e env (lambda (a)
                                             (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))) k))))
      (`(var ,y) (apply-env env y k))
      (`(lambda ,body) (apply-k k (lambda (a k^)
                                    (value-of-cps body (lambda (y k^^) (if (zero? y) (apply-k k^^ a) (apply-env env (sub1 y) k^^))) k^))))
      (`(app ,rator ,rand) (value-of-cps rator env (lambda (f)
                                                     (value-of-cps rand env (lambda (w)
                                                                              (apply-closure f w k)))))))))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of-cps "unbound identifier"))))

#;(define extend-env
  (λ (x arg env k)
    (λ (y k^)
      (cond
        ((zero? y) (apply-k k^ arg))
        (else (apply-env env y k))))))

(define apply-env
  (λ (env y k)
    (k (env y))))

(define apply-closure
  (λ (clos arg k)
    (clos arg k)))

(define apply-k
  (λ (k v)
    (k v)))

 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))
