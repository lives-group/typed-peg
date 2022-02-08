#lang racket

(require parser-tools/yacc
         typed-peg/core
         typed-peg/lexer)

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
    (term [(NOT term) (pneg $2)]
          [(factor)   $1])
    (factor [(factor STAR) (pstar $1)]
            [(atom) $1])
    (atom [(EPSILON) (peps)]
          [(CHAR)    (pchr (car (string->list $1)))]
          [(VAR)     (pvar $1)]
          [(LPAREN expr RPAREN) $2])
    )))

(define (parse ip)
  (port-count-lines! ip)  
  (core-parser (lambda () (next-token ip))))

(provide parse)
