#lang racket

(require (lib "eopl.ss" "eopl"))
(provide (all-defined-out))


(define-datatype program program?
  (a-program
   (stmnts (list-of statement?))))

(define-datatype statement statement?
  (compound-stmt
   (stmnt compound-statement?))
  (simple-stmt
   (stmnt simple-statement?)))

(define-datatype compound-statement compound-statement?
  (function-def
   (id symbol?)
   (params (list-of parameter?))
   (body (list-of statement?)))
  (if-stmt
   (condition expression?)
   (body (list-of statement?))
   (else-block (list-of statement?)))
  (for-stmt
   (id symbol?)
   (exp expression?)
   (body (list-of statement?)))
  )

(define-datatype parameter parameter?
  (param-with-default
   (id symbol?)
   (default expression?)))

(define-datatype simple-statement simple-statement?
  (assign-stmt
   (id symbol?)
   (right-hand expression?))
  (global-stmt
   (id symbol?))
  (return-stmt
   (exp expression?))
  (print-stmt
   (atoms (list-of expression?)))
  (pass-stmt)
  (break-stmt)
  (continue-stmt))

(define-datatype expression expression?
  (disjunc (arg disjunction?)))
  
(define-datatype disjunction disjunction?
  (simple-disjunct
   (arg conjunction?))
  (compound-disjunct
   (arg1 disjunction?)
   (arg2 conjunction?)))

(define-datatype conjunction conjunction?
  (simple-conjunct
   (arg inversion?))
  (compound-conjunct
   (arg1 conjunction?)
   (arg2 inversion?))
  )

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
  (simple-comp
   (sum1 sum?)))

(define-datatype sum sum?
  (addition-sum
   (left sum?)
   (right term?))
  (subtraction-sum
   (left sum?)
   (right term?))
  (simple-sum
   (t term?)))

(define-datatype term term?
  (mult-factor
   (left term?)
   (right-hand factor?))
  (div-factor
   (left term?)
   (right factor?))
  (simple-term
   (x factor?))
  )

(define-datatype factor factor?
  (plus-factor
   (x factor?))
  (minus-factor
   (x factor?))
  (simple-factor
   (x power?)))

(define-datatype power power?
  (to-power
   (left atom?)
   (right factor?))
  (simple-power
   (x primary?)))

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
   (id symbol?))
  (atom-bool
   (val symbol?))
  (atom-none)
  (atom-number
   (val number?))
  (atom-list
   (val (list-of expression?))))

(define-datatype thunk thunk?
  (a-thunk
   (expression expression?)
   (environment list?)
   (saved-store list?)))

(define-datatype proc proc?
  (procedure
   (id symbol?)
   (params list?)
   (body list?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst list?))
  (proc-val
   (proc proc?))
  (none-val))

(define (expval->val exp1)
  (cases expval exp1
    (list-val (lst) lst)
    (bool-val (bool) bool)
    (num-val (num) num)
    (none-val () 'None)
    (proc-val (p) p)))


