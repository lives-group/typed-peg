#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens
  (CHAR VAR))

(define-empty-tokens op-tokens
  (EOF OR LPAREN RPAREN STAR NOT SEMI EPSILON ARROW START))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace #\newline) (return-without-pos (next-token input-port))]
   ["/" (token-OR)]
   ["*" (token-STAR)]
   ["<--" (token-ARROW)]
   ["!" (token-NOT)]
   [";" (token-SEMI)]
   ["epsilon" (token-EPSILON)]
   ["start:" (token-START)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [(:seq alphabetic (:* (:+ alphabetic numeric)))
    (token-VAR lexeme)]
   [(:seq #\' any-char #\') (token-CHAR (let* ([s lexeme]
                                               [n (string-length s)])
                                          (substring s 1 (- n 1))))]))


(provide value-tokens op-tokens next-token)
