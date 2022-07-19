#lang eopl

(define-datatype program program?
  (a-program
   (stmnts (list-of statement?))))

(define-datatype statement statement?
  (compound
   (stmnt compound-statement?))
  (simple
   (stmnt simple-statement?)))

(define-datatype compound-statement compound-statement?
  (function-def
   (id string?)
   (params (list-of parameter?))
   (body (list-of statement?)))
  (if-stmt
   (exp expression?)
   (stmts (list-of statement?))
   (else-block (list-of statement?)))
  (for-stmt
   (id string?)
   (exp expression?)
   (stmts (list-of statement?))))

(define-datatype parameter parameter?
  (param-with-default
   (id string?)
   (default expression?)))

(define-datatype simple-statement simple-statement?
  (assignment
   (id string?)
   (exp expression?))
  (global-stmt
   (id string?))
  (return-stmt
   (exp expression?))
  (pass)
  (break)
  (continue))

(define-datatype expression expression?
  (disjunctions (list-of conjunction?)))

(define-datatype conjunction conjunction?
  (inversions (list-of inversion?)))

(define-datatype inversion inversion?
  (not-inv
   (inv inversion?))
  (comp-inv
   (comp comparison?)))

(define-datatype comparison comparison?
  (eq-sum
   (sum1 sum?)
   (sum2 sum?))
  (lt-sum
   (sum1 sum?)
   (sum2 sum?))
  (gt-sum
   (sum1 sum?)
   (sum2 sum?))
  (simple-sum
   (sum1 sum?)))

(define-datatype sum sum?
  (terms (list-of term?)))

(define-datatype term term?
  (factors (list-of factor?)))

(define-datatype factor factor?
  (positive-factor
   (arg power?))
  (negative-factor
   (arg power?)))

(define-datatype power power?
  (power1
   (arg1 atom?)
   (arg2 factor?))
  (power2
   (arg1 primary?)))

(define-datatype primary primary?
  (primary-simple
   (arg atom?))
  (primary-indexed
   (arr primary?)
   (index expression?))
  (primary-called
   (func primary?)
   (args (list-of expression?))))

(define-datatype atom atom?
  (atom-var
   (id string?))
  (atom-bool
   (val boolean?))
  (atom-none)
  (atom-number
   (val number?))
  (atom-list
   (val (list-of expression?))))

 (provide (all-defined-out))