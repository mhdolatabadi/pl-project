#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme))
   )
   (";" (token-semicolon))
   ("pass" (token-pass))
   ("break" (token-break))
   ("continue" (token-continue))
   ("=" (token-assignment))
   ("return" (token-return))
   ("global" (token-global))
   ("def" (token-def))
   ("(" (token-openParenthesis))
   (")" (token-closeParenthesis))
   (":" (token-colon))
   ("," (token-comma))
   ("if" (token-if))
   ("else" (token-else))
   ("for" (token-for))
   ("in" (token-in))
   ("or" (token-or))
   ("and" (token-and))
   ("not" (token-not))
   ("==" (token-equals))
   ("<" (token-lessThan))
   (">" (token-greaterThan))
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-mul))
   ("/" (token-div))
   ("**" (token-pow))
   ("[" (token-openBracket))
   ("]" (token-closeBracket))
   ("True" (token-true))
   ("False" (token-false))
   ("None" (token-none))
   ((:+(:or (char-range #\a #\z)
            (char-range #\A #\Z)))
    (token-ID lexeme)
   )
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM ID))
(define-empty-tokens b (semicolon pass break continue
                        assignment return global def
                        openParenthesis closeParenthesis
                        colon comma if else for in or
                        and not equals lessThan greaterThan
                        plus minus mul div pow openBracket
                        closeBracket true false none EOF))


(define simple-math-parser
  (parser
   (start Statements)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (Statements
         ((Statement semicolon) (list 'statement $1))
         ((Statements Statement semicolon) (list 'statements $1 $2)))
    (Statement
         ((Compound-stmt) (list 'compound-stmt $1))
         ((Simple-stmt) (list 'simple-stmt $1)))
    (Simple-stmt
         ((Assignment) (list 'assignment $1))
         ((Global-stmt) (list 'global-stmt $1))
         ((Return-stmt) (list 'return-stmt $1))
         ((pass) (list 'pass))
         ((break) (list 'break))
         ((continue) (list 'continue)))
    (Compound-stmt
         ((Function-def) (list 'function-def $1))
         ((If-stmt) (list 'if-stmt $1))
         ((For-stmt) (list 'for-stmt $1)))
    (Assignment
         ((ID assignment Expression) (list 'assignment $1 $3)))
    (Return-stmt
         ((return) (list 'return-none))
         ((return Expression) (list 'return $2)))
    (Global-stmt
         ((global ID) (list 'global $2)))
    (Function-def
         ((def ID openParenthesis Params closeParenthesis colon Statements) (list 'function-with-params $2 $4 $7))
         ((def ID openParenthesis closeParenthesis colon Statements) (list 'function-without-params $2 $6)))
    (Params
         ((Param-with-default) (list 'param-with-default $1))
         ((Params comma Param-with-default) (list 'param-list $1 $3)))
    (Param-with-default
         ((ID assignment Expression) (list 'param-init $1 $3)))
    (If-stmt
         ((if Expression colon Statements Else-block) (list 'if-block $2 $4 $5)))
    (Else-block
         ((else colon Statements) (list 'else-block $3)))
    (For-stmt
         ((for ID in Expression colon Statements) (list 'for-block $2 $4 $6)))
    (Expression
         ((Disjunction) (list 'disjunction $1)))
    (Disjunction
         ((Conjunction) (list 'conjunction $1))
         ((Disjunction or Conjunction) (list 'disjunction-or-conjunction $1 $3)))
    (Conjunction
         ((Inversion) (list 'inversion $1))
         ((Conjunction and Inversion) (list 'conjunction-and-inversion $1 $3)))
    (Inversion
         ((not Inversion) (list 'not-inversion $2))
         ((Comparison) (list 'comparison $1)))
    (Comparison
         ((Eq-sum) (list 'eq-sum $1))
         ((Lt-sum) (list 'lt-sum $1))
         ((Gt-sum) (list 'gt-sum $1))
         ((Sum) (list 'sum $1)))
    (Eq-sum
         ((Sum equals Sum) (list 'equals $1 $3)))
    (Lt-sum
         ((Sum lessThan Sum) (list 'less-than $1 $3)))
    (Gt-sum
         ((Sum greaterThan Sum) (list 'greater-than $1 $3)))
    (Sum
         ((Sum plus Term) (list 'plus-sum $1 $3))
         ((Sum minus Term) (list 'minus-sum $1 $3))
         ((Term) (list 'term $1)))
    (Term
         ((Term mul Factor) (list 'mul-term $1 $3))
         ((Term div Factor) (list 'div-term $1 $3))
         ((Factor) (list 'factor $1)))
    (Factor
         ((plus Power) (list 'plus-power $2))
         ((minus Power) (list 'minus-power $2))
         ((Power) (list 'power $1)))
    (Power
         ((Atom pow Factor) (list 'atom-to-power $1 $3))
         ((Primary) (list 'power-primary $1)))
    (Primary
         ((Atom) (list 'primary-atom $1))
         ((Primary openBracket Expression closeBracket) (list 'primary-bracket $1 $3))
         ((Primary openParenthesis closeParenthesis) (list 'primary-no-arg-call $1))
         ((Primary openParenthesis Arguments closeParenthesis) (list 'primary-arg-call $1 $3)))
    (Arguments
         ((Expression) (list 'expression-argument $1))
         ((Arguments comma Expression) (list 'argument-list $1 $3)))
    (Atom
         ((ID) (list 'atom-id $1))
         ((true) (list 'atom-true))
         ((false) (list 'atom-false))
         ((none) (list 'atom-none))
         ((NUM) (list 'atom-num $1))
         ((List) (list 'atom-list $1)))
    (List
         ((openBracket Expressions closeBracket) (list 'list $2))
         ((openBracket closeBracket) (list 'empty-list)))
    (Expressions
         ((Expressions comma Expression) (list 'expression-list $1 $3))
         ((Expression) (list 'last-expression $1)))
   )
  )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

