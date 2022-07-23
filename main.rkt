#lang racket

(require "./evaluator.rkt")
(require "./myLexer.rkt")
(require "./myParser.rkt")
(require "./datatypes.rkt")

(define run
  (lambda (file-path)
    (value-of-program (parse file-path))))

(define program "sample.txt")
