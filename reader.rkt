#lang racket

(require typed-peg/grammar
         typed-peg/typing/infer
         typed-peg/parser
         typed-peg/pretty
         typed-peg/tree
         syntax/strip-context)


(provide (rename-out [peg-read read]
                     [peg-read-syntax read-syntax]))

(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (let ([types (infer grammar)])
    (if (eq? (cdr types) 'unsat)
        (displayln "The grammar isn't well-typed! It can loop on some inputs.")
        (datum->syntax
         #f
         `(module peg-mod racket
            (provide parser
                     pretty
                     (all-from-out typed-peg/tree))

            (require typed-peg/parser
                     typed-peg/pretty
                     typed-peg/tree
                     typed-peg/typing/infer)

            (define (parser s)
              (peg-parse ,grammar s))
            (define (pretty t)
              (peg-pretty ,grammar t)))))))
