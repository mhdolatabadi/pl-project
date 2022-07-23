#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

(define my-simple-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ("\"" (token-doubleQuote))
   ("evaluate" (token-evaluate))
   (";" (token-semicolon))
   ("pass" (token-pass))
   ("break" (token-break))
   ("continue" (token-continue))
   ("print" (token-print))
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
    (token-ID lexeme))
   ((:: "\"" (complement "\"") "\"")
    (token-PATH lexeme))
   (whitespace (my-simple-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM ID PATH))
(define-empty-tokens b (semicolon pass break continue print
                        assignment return global def
                        openParenthesis closeParenthesis
                        colon comma if else for in or
                        and not equals lessThan greaterThan
                        plus minus mul div pow openBracket
                        closeBracket true false none EOF
                        doubleQuote evaluate))


;test
;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2.577+ 3 if fast Fish 4")))
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
