#lang racket

(require "../lexer.rkt"
         syntax/strip-context
         parser-tools/lex)

(define (peg-lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))


(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (peg-read-syntax path port)
  (datum->syntax
   #f
   `(module peg-tokenize-mod racket
      ,(peg-lex-test port))))

(module+ reader (provide (rename-out [peg-read read]
                                     [peg-read-syntax read-syntax])))
