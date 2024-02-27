#lang racket
(require racket/trace)
;Problem 1
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                ((null? ls) '())
                ((eqv? 0 (car ls)) (k (cdr ls)))
                (else (cons (car ls) (last-non-zero (cdr ls))))))))
        (k (last-non-zero ls))))))



(last-non-zero '(0))
(last-non-zero '(1 2 3 0 4 5))
(last-non-zero '(1 0 2 3 0 4 5))
(last-non-zero '(1 2 3 4 5))


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
      (`,y #:when (symbol? y)
           (let ([idx (index y acc)])
             (cond
               ((symbol? idx) idx)
               ((number? idx) `(var ,idx)))))
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc))))
      (`(,rator ,rand)
       `(,(lex rator acc) ,(lex rand acc))))))