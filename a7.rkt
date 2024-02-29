#lang racket
;Problem 1
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                ((null? ls) '())
                ((eqv? 0 (car ls)) (k (last-non-zero(cdr ls))))
                (else (cons (car ls) (last-non-zero (cdr ls))))))))
        (last-non-zero ls)))))

#|
(last-non-zero '(0))
(last-non-zero '(1 2 3 0 4 5))
(last-non-zero '(1 0 2 3 0 4 5))
(last-non-zero '(1 2 3 4 5))
|#


;Problem 2
(define index
  (λ (var ls)
    (match ls
      (`() var)
      (`(,a . ,_) #:when (eqv? var a) 0)
      (`(,_ . ,d) (let ([idx (index var d)])
                    (cond
                      ((number? idx) (add1 idx))
                      ((symbol? idx) var)))))))

(define lex
  (λ (exp acc)
    (match exp
      [`,y #:when (symbol? y)
           (let ([idx (index y acc)])
             (cond
               ((symbol? idx) idx)
               ((number? idx) `(var ,idx))))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc)))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(catch ,k-name ,body) `(catch ,(lex body (cons k-name acc)))]
      [`(pitch ,k-expr ,expr) `(pitch ,(lex k-expr acc) ,(lex expr acc))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))

#|
(lex '(lambda (x) x)'())
(lex '(lambda (y) (lambda (x) y))'())
(lex '(lambda (y) (lambda (x) (x y)))'())
(lex '(lambda (x) (lambda (x) (x x)))'())
(lex '(lambda (x) (lambda (x) (y x)))'())
|#


(require rackunit)

;Problem 3
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,n) (apply-k k n))
      (`(mult ,x1 ,x2) (* (value-of-cps x1 env k) (value-of-cps x2 env k)))
      (`(sub1 ,x) (sub1 (value-of-cps x env k)))
      (`(zero ,x) (zero? (value-of-cps x env k)))
      (`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v)
                                                         (if v
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)))))
      (`(catch ,body) 
       (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ k) (apply-env env (sub1 y)))) k))
      (`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k^)
                                                        (value-of-cps v-exp env (lambda (v)
                                                                                  (k^ v))))))
      (`(let ,e ,body) (value-of-cps e env (lambda (a)
                                             (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))) k))))
      (`(var ,y) (apply-env env y k))
      (`(lambda ,body) (apply-k k (lambda (a k^)
                                    (value-of-cps body (lambda (y k^^) (if (zero? y) (apply-k k^^ a) (apply-env env (sub1 y) k^^))) k^))))
      (`(app ,rator ,rand) (value-of-cps rator env (lambda (f)
                                                     (value-of-cps rand env (lambda (w)
                                                                              (f w k)))))))))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of-cps "unbound identifier"))))

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



;Test Cases
#|
(check-equal? (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(check-equal? (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 5))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k)) 15)
(check-equal? (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda
                                          (if (zero (var 0))
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1)
|#