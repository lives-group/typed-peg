#lang racket

(require parser-tools/yacc
         typed-peg/core
         typed-peg/lexer)

;; converting a string token into a tree of
;; characters concatenation

(define (string->tree s)
  (match s
    ['() (peps)]
    [(cons c '()) (pchr c)]
    [(cons c s1) (pcat (pchr c)
                       (string->tree s1))]))

(define core-parser
  (parser
   (start peg)
   (end EOF)
   (tokens value-tokens op-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin (printf "parse error:\na = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e)
             (void))))
   (grammar
    (peg [(rules START expr) (peg-grammar $1 $3)])
    (rules [() '()]
           [(rule rules) (cons $1 $2)])
    (rule [(VAR ARROW expr SEMI) (cons $1 $3)])
    (expr [(cat OR expr) (pchoice $1 $3)]
          [(cat) $1])
    (cat [(cat term) (pcat $1 $2)]
         [(term) $1])
    (term [(prefixop term) ($1 $2)]
          [(factor)   $1])
    (prefixop [(NOT) (lambda (e) (pneg e))]
              [(AND) (lambda (e) (pneg (pneg e)))])
    (factor [(factor postfix) ($2 $1)]
            [(atom) $1])
    (postfix [(STAR) (lambda (e) (pstar e))]
             [(PLUS) (lambda (e) (pcat e (pstar e)))]
             [(OPTION) (lambda (e) (pchoice e peps))])
    (char-list [(CHAR) (pchr (car (string->list $1)))]
               [(CHAR COMMA char-list) (pchoice $1 $3)])
    (atom [(EPSILON) (peps)]
          [(CHAR)    (pchr (car (string->list $1)))]
          [(STRING)  (string->tree (string->list $1))]
          [(LBRACK char-list RBRACK) $2]
          [(ANY)     (pany)]
          [(VAR)     (pvar $1)]
          [(LPAREN expr RPAREN) $2])
    )))

(define (parse ip)
  (port-count-lines! ip)  
  (core-parser (lambda () (next-token ip))))

(provide parse)
