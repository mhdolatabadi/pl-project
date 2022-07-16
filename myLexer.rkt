#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
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
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM))
(define-empty-tokens b (semicolon pass break continue
                        assignment return global def
                        openParenthesis closeParenthesis
                        colon comma if else for in or
                        and not equals lessThan greaterThan
                        plus minus mul div pow openBracket
                        closeBracket true false none EOF))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2.577+ 3 +-   4")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
