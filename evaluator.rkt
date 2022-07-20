#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")
(require "./myLexer.rkt")
(require "./myParser.rkt")

(define (empty-environment) '())
(define the-store '())

(define (extend-environment var ref env) (cons (list var ref) env))

(define (apply-environment var env)
  (if (null? env) '()
      (if (eqv? var (caar env))
          (cadar env)
          (apply-environment var (cdr env)))))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (setref! ref new-val)
  (set! the-store (list-set the-store ref new-val)))


(define value-of-program
  (lambda (prgrm)
    (cases program prgrm
      (a-program (stmts) (begin
                           (define env (empty-environment))
                           (value-of-statements stmts env))))))


(define (value-of-statements statements env)
  (if (null? statements) env
      (let ((env-res (value-of-statement (car statements) env)))
        (if (null? env-res)
            (value-of-statements (cdr statements) env-res)
            (if (eqv? 'Return (car env-res)) env-res
                (value-of-statements (cdr statements) env-res))))))


(define (value-of-statement stmt env)
  (cases statement stmt
    (simple-stmt (stmt)
                 (value-of-simple-statement stmt env))
    (compound-stmt (stmt) void)))


(define (value-of-simple-statement s env)
  (cases simple-statement s
    (assign-stmt (id right-hand)
                 (value-of-assignment-statement id right-hand env))
    (return-stmt (body) (value-of-return-statement body env))
    (global-stmt (id) void)
    (pass-stmt () env)
    (break-stmt () env)
    (continue-stmt () env)))


(define (value-of-assignment-statement id right-hand env)
  (let ((bound-val (apply-environment id env)))
    (if (null? bound-val)
        (let ((ref (newref (a-thunk right-hand env the-store))))
          (extend-environment id ref env))
        (begin
          (setref! bound-val (a-thunk right-hand env the-store))
          env))))


(define (value-of-return-statement body env)
  (if (null? body)
      (list 'Return (none-val))
      (list 'Return (value-of-expression body env))))


(define (value-of-expression exp env) null)

(define (value-of-global-statement id env)
  (let ((ref (apply-environment id (get-global-environment env))))
    (extend-environment id ref env)))

(define (get-global-environment env)
  (if (null? env) env
      (if (has-scope env)
          (if (and (null? (cdar env)) (eqv? (caar env)) 'global_scope)
              (cdr env)
              (get-global-environment (cdr env)))
          env)))

(define (has-scope env)
  (if (null? env) #f
      (if (and
           (null? (cdar env))
           (or
            (eqv? (caar env) 'local_scope)
            (eqv? (caar env) 'global_scope)))
          #t
          (has-scope (cdr env)))))


;(define envtest (value-of-program test1))
;;(value-of-thunk (third the-store) envtest)
(provide value-of-program)