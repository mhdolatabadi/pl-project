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


(define value-of-statement
  (lambda (exp env)
    (let [(type (car exp))
          (stmt (cadr exp))]
      (cond
        [(eqv? type 'compound-stmt)
         (value-of-compound-statement stmt env)]
        [(eqv? type 'simple-stmt)
         (value-of-simple-statement stmt env)]
        [else void]))))

(define value-of-compound-statement
  (lambda (exp env)
    (let [(type (car exp))
          (stmt (cadr exp))]
      (cond
        [(eqv? type 'function-def)
         (value-of-function-def stmt env)]
        [(eqv? type 'if-stmt)
         (value-of-if-stmt stmt env)]
        [(eqv? type 'for-stmt)
         (value-of-for-stmt stmt env)]
        [else void]))))

(define value-of-simple-statement
  (lambda (exp env)
    (let [(type (car exp))
          (stmt (cadr exp))]
      (cond
        [(eqv? type 'assignment)
         (print "assignment")]
        [(eqv? type 'global-stmt)
         (print "global")]
        [(eqv? type 'return-stmt)
         (print "return")]
        [(eqv? type 'pass)
         (print "pass")]
        [(eqv? type 'break)
         (print "break")]
        [(eqv? type 'continue)
         (print "continue")]
        [else void]))))
