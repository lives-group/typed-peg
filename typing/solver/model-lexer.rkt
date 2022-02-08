#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide ident-token
         str-token
         syntax-tokens
         next-token)

(define-tokens ident-token (ID))
(define-tokens str-token (STRING))
(define-empty-tokens syntax-tokens
  (EOF SAT UNSAT LPAREN RPAREN DEFINE TYPE
       MK-TYPE TRUE FALSE NT AS CONST SET STORE
       MAP OR UNDER BOOL ERROR))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace #\newline)
    (return-without-pos (next-token input-port))]
   ["sat" (token-SAT)]
   ["unsat" (token-UNSAT)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["define-fun" (token-DEFINE)]
   ["Type" (token-TYPE)]
   ["mk-type" (token-MK-TYPE)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["NT" (token-NT)]
   ["as" (token-AS)]
   ["const" (token-CONST)]
   ["Set" (token-SET)]
   ["store" (token-STORE)]
   ["map" (token-MAP)]
   ["or" (token-OR)]
   ["_" (token-UNDER)]
   ["Bool" (token-BOOL)]
   ["error" (token-ERROR)]
   [(:seq #\" (:* any-char) #\")
    (token-STRING lexeme)]
   [(:seq alphabetic (:* (:+ numeric alphabetic)))
    (token-ID lexeme)]))
