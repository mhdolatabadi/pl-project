#lang racket

(define value-of
  (lambda (exp env)
    (let [(rule (car exp))]
      (cond
        [(eqv? rule 'statements) (value-of-statements exp env)]
        [else void]))))

(define value-of-statements
  (lambda (exp env)
    (let [(stmnts (cadr exp))
          (stmnt (caddr exp))]
      (begin
        (value-of stmnts env)
        (value-of-statement stmnt env)))))
