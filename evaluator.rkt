#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")
(require "./myLexer.rkt")
(require "./myParser.rkt")

(define (empty-environment) '())
(define the-store '())

(define (extend-environment var ref env) (cons (list var ref) env))

(define (apply-environment var env)
  (if (or
       (null? env)
       (and
        (null? (cdar env))
        (or (eqv? (caar env) 'local_scope)
            (eqv? (caar env) 'global_scope))))
      '()
      (if (eqv? var (caar env))
          (cadar env)
          (apply-environment var (cdr env)))))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (setref! ref new-val)
  (set! the-store (list-set the-store ref new-val)))

(define (deref ref)
   (list-ref the-store ref))


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
    (compound-stmt (stmt)
                 (value-of-compound-statement stmt env))))


(define (value-of-simple-statement s env)
  (cases simple-statement s
    (assign-stmt (id right-hand)
                 (value-of-assignment-statement id right-hand env))
    (return-stmt (body) (value-of-return-statement body env))
    (global-stmt (id) (value-of-global-statement id env))
    (print-stmt (exps) (begin
                          (println
                           (map (lambda (x)
                                  (expval->val (value-of-expression x env))) exps))
                          env))
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


(define (value-of-expression exp env)
  (cases expression exp
    (disjunc (body) (value-of-disjunction body env))))


(define (value-of-disjunction body env)
  (cases disjunction body
    (simple-disjunct (body) (value-of-conjunction body env))
    (compound-disjunct (body-disjunc body-conjunc)
                       (let ((val1 (value-of-disjunction body-disjunc env))
                             (val2 (value-of-conjunction body-conjunc env)))
                         (if (expval->val val1) val1 val2)))))


(define (value-of-conjunction body env)
  (cases conjunction body
    (simple-conjunct (x) (value-of-inversion x env))
    (compound-conjunct (body-conjunc body-inv)
                       (let ((val1 (value-of-conjunction body-conjunc env))
                             (val2 (value-of-inversion body-inv env)))
                         (if (not (expval->val val1)) val1 val2)))))


(define (value-of-inversion body env)
  (cases inversion body
    (not-inv (inv) (bool-val (not (expval->val (value-of-inversion inv env)))))
    (comp-inv (comp) (value-of-comparison comp env))))


(define (value-of-comparison body env)
  (cases comparison body
    (eq-sum (sum1 sum2) (bool-val (= (expval->val (value-of-sum sum1 env))
                                     (expval->val (value-of-sum sum2 env)))))
    (lt-sum (sum1 sum2) (bool-val (< (expval->val (value-of-sum sum1 env))
                                     (expval->val (value-of-sum sum2 env)))))
    (gt-sum (sum1 sum2) (bool-val (> (expval->val (value-of-sum sum1 env))
                                     (expval->val (value-of-sum sum2 env)))))
    (simple-comp (sum1) (value-of-sum sum1 env))))


(define (value-of-sum body env)
  (cases sum body
    (addition-sum (left right) (addition (value-of-sum left env)
                                         (value-of-term right env)))
    (subtraction-sum (left right)
                     (num-val (- (expval->val (value-of-sum left env))
                                 (expval->val (value-of-term right env)))))
    (simple-sum (term) (value-of-term term env))))


(define (addition left-hand right-hand)
  (cases expval left-hand
    (num-val (num) (num-val (+ (expval->val right-hand) num)))
    (list-val (lst) (list-val (append lst (expval->val right-hand))))
    (else (none-val))))


(define (value-of-term body env)
  (cases term body
    (mult-factor (left right)
                 (num-val (* (expval->val (value-of-term left env))
                             (expval->val (value-of-factor right env)))))
    (div-factor (left right)
                (num-val (/ (expval->val (value-of-term left env))
                            (expval->val (value-of-factor right env)))))
    (simple-term (factor) (value-of-factor factor env))))


(define (value-of-factor body env)
  (cases factor body
    (plus-factor (x) (value-of-factor x env))
    (minus-factor (x) (num-val (- (expval->val (value-of-factor x env)))))
    (simple-factor (x) (value-of-power x env))))


(define (value-of-power body env)
  (cases power body
    (to-power (left right) (powering left right env))
    (simple-power (primary) (value-of-primary primary env))))


(define (powering left right env)
  (let ((right-val (expval->val (value-of-factor right env)))
        (left-val (expval->val (value-of-atom left env))))
    (num-val (expt left-val right-val))))


(define (value-of-atom body env)
  (cases atom body
    (atom-var (var) (value-of-atom-var var env))
    (atom-bool (bool) (bool-val (eqv? bool 'True)))
    (atom-none () (none-val))
    (atom-number (number) (num-val number))
    (atom-list (lst) (list-val lst))))




(define (value-of-atom-var var env)
  (let ((value (deref (apply-environment var env))))
    (if (thunk? value)
        (cases thunk value
          (a-thunk (exp1 prev-env saved-store)
                   (let ((curr-store the-store))
                     (set! the-store saved-store)
                     (let ((result (value-of-expression exp1 prev-env)))
                       (begin
                         (set! the-store curr-store)
                         result))))) value)))


(define (value-of-primary body env)
  (cases primary body
    (primary-simple (atom) (value-of-atom atom env))
    (primary-indexed (arr index)
                     (list-ref (expval->val (value-of-primary arr env))
                               (expval->val (value-of-expression index env))))
    (primary-called (func args) (value-of-function-call func args env))))


(define (value-of-function-call func args env)
  (let ((val (expval->val (value-of-primary func env))))
    (cases proc val
      (procedure (id params body)
                 (apply-procedure id params body args
                                  (add-environment-scope env) env)))))


(define (environment-contains-global-scope env)
  (if (null? env) #f
      (if (eqv? (caar env) 'global_scope) #t
          (environment-contains-global-scope (cdr env)))))


(define (add-environment-scope env)
  (if (environment-contains-global-scope env)
      (cons (list 'local_scope) env)
      (cons (list 'global_scope) env)))


(define (apply-procedure id params body args env prev-env)
  (value-of-function-body body (extend-environment id
                                                   (newref (proc-val (procedure id params body)))
                                                   (extend-env-with-args params args env prev-env))))


(define (value-of-function-body statements env)
  (let ((res (value-of-statements statements env)))
    (if (null? res)
        (none-val)
        (if (eqv? 'Return (car res))
            (cadr res) 
            (none-val)))))


(define (extend-env-with-args params args env prev-env)
  (if (null? args)
      (extend-env-only-params params env prev-env)
      (extend-env-with-args (cdr params) (cdr args)
                            (extend-env-with-arg (car params) (car args) env prev-env) prev-env)))


(define (extend-env-with-arg param1 arg1 env prev-env)
  (cases parameter param1
    (param-with-default
     (id exp1)
     (extend-environment id (newref (a-thunk arg1 prev-env the-store)) env))))


(define (extend-env-only-params params env prev-env)
  (if (null? params) env
      (cases parameter (car params)
        (param-with-default (id exp1)
                            (extend-env-only-params
                             (cdr params)
                             (extend-environment id (newref (a-thunk exp1 prev-env the-store)) env) prev-env)))))

  
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
           (or (eqv? (caar env) 'local_scope)
               (eqv? (caar env) 'global_scope))) #t
           (has-scope (cdr env)))))


(define (value-of-compound-statement stmt env)
  (cases compound-statement stmt
    (function-def (id params body)
                  (value-of-function-def id params body env))
    (if-stmt (condition body else-block)
             (value-of-if-statement condition body else-block env value-of-statements))
    (for-stmt (id exp body)
             (value-of-for id exp body env))))

(define (value-of-function-def id params body env)
  (let ((ref (newref (proc-val (procedure id params body)))))
    (extend-environment id ref env)))


(define (value-of-if-statement condition body else-block env stmts-evaluator)
  (let ((res (value-of-expression condition env)))
    (if (expval->val res)
        (stmts-evaluator body env)
        (stmts-evaluator else-block env))))


(define (value-of-for id exp body env)
  (let ((exp-val (reverse (expval->val (value-of-expression exp env)))))
        (let ((result (value-of-for-body id exp-val body env)))
          (if (or (eqv? result 'break) (eqv? result 'continue)) env result))))


(define (value-of-for-body id iterable body env)
  (if (null? iterable) env
      (begin
        (let ((result (value-of-for-body id (cdr iterable) body env)))
          (cond
            [(eqv? result 'break) 'break]
            [(eqv? result 'continue)
             (value-of-for-statements
              body (extend-environment id (newref (a-thunk (car iterable) env the-store)) env))]
            [(null? result)
             (value-of-for-statements
              body (extend-environment id (newref (a-thunk (car iterable) env the-store)) env))]
            [(eqv? 'Return (car result)) result]
            [else
             (value-of-for-statements
              body (extend-environment id (newref (a-thunk (first iterable) env the-store)) env))])))))


(define (value-of-for-statements statements env)
  (if (null? statements) env
      (let ((stmt (car statements)))
        (cases statement stmt
          (simple-stmt (s)
            (cases simple-statement s
              (break-stmt () 'break)
              (continue-stmt () 'continue)
              (return-stmt (exp) (value-of-return-statement exp env))
              (else (begin
                      (value-of-for-statements
                       (cdr statements)
                       (value-of-statement stmt env))
                      env))))
          (compound-stmt (s)
            (cases compound-statement s
              (if-stmt (condition body else-block)
                       (let ((result (value-of-if-statement
                                      condition body else-block env value-of-for-statements)))
                         (cond
                           [(eqv? result 'break) 'break]
                           [(eqv? result 'continue) 'continue]
                           [(null? result)
                            (value-of-for-statements (cdr statements) result)]
                           [(eqv? 'Return (car result)) result]
                           [else (value-of-for-statements (cdr statements) result)])))
              (else (begin
                      (value-of-for-statements
                      (cdr statements)
                      (value-of-statement stmt env))
                      env))))))))

;(define envtest (value-of-program test1))
;;(value-of-thunk (third the-store) envtest)
(provide value-of-program)