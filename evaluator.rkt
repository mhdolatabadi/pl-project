#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")
(require "./myLexer.rkt")
(require "./myParser.rkt")

(define (empty-environment) (list))

(define (extend-environment var ref env) (cons (list var ref) env))


(define value-of-program
  (lambda (prgrm)
    (cases program prgrm
      (a-program (stmts) (begin
                           (define env (empty-environment))
                           (print "salam"))))))



;(define envtest (value-of-program test1))
;;(value-of-thunk (third the-store) envtest)
(provide value-of-program)