#lang racket

(require parser-tools/yacc
         typed-peg/typing/solver/model-lexer
         typed-peg/typing/type)

(provide parse)

(define model-parser
  (parser
   (start input)
   (end EOF)
   (tokens ident-token
           str-token
           syntax-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin
        (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n"
                a
                b
                c
                d
                e)
        (void))))
   (grammar
    (input [(UNSAT LPAREN ERROR STRING RPAREN) 'unsat]
           [(SAT LPAREN def-list RPAREN) $3])
    (def-list [() '()]
              [(def def-list) (cons $1 $2)])
    (def [(LPAREN DEFINE ID params type body RPAREN) (cons $3 $6)])
    (params [(LPAREN arg-list RPAREN) $2])
    (type [(TYPE) '()]
          [(LPAREN SET NT RPAREN) '()])
    (body [(LPAREN expr RPAREN) $2])
    (expr [(mk-type) $1]
          [(LPAREN AS CONST LPAREN SET NT RPAREN RPAREN FALSE) '()])
    (arg-list [() '()]
              [(arg arg-list) (cons $1 $2)])
    (arg [(LPAREN ID type RPAREN) (cons $2 $3)])
    (mk-type [(MK-TYPE is-null head-set) (type $2 $3)])
    (is-null [(TRUE) #t]
             [(FALSE) #f])
    (head-set [(LPAREN LPAREN AS CONST LPAREN SET NT RPAREN RPAREN FALSE RPAREN) '()]
              [(LPAREN STORE head-set ID TRUE RPAREN) (cons $4 $3)]
              [(LPAREN union head-set head-set head-set-list RPAREN) (append $3 $4 $5)])
    (head-set-list [() '()]
                   [(head-set head-set-list) (append $1 $2)])
    (union [(LPAREN UNDER MAP or-def RPAREN) '()])
    (or-def [(LPAREN OR LPAREN BOOL BOOL RPAREN BOOL RPAREN) '()]))))

(define (parse ip)
  (port-count-lines! ip)
  (model-parser (lambda () (next-token ip))))
