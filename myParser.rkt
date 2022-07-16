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
   )
  )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

