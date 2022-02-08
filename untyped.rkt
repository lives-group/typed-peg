#lang racket

(module reader racket 
(require typed-peg/grammar)


(provide (rename-out [peg-read read]
                     [peg-read-syntax read-syntax]))

(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (datum->syntax
   #f
   `(module peg-mod racket
      (provide parser
               pretty
               (all-from-out typed-peg/tree))
      
      (require typed-peg/parser
               typed-peg/pretty
               typed-peg/tree)
      
      (define (parser s)
        (peg-parse ,grammar s))
      (define (pretty t)
        (peg-pretty ,grammar t)))))
)
