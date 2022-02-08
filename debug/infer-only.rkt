#lang racket

(module reader racket
  (require typed-peg/grammar
           typed-peg/typing/infer
           typed-peg/typing/constraint
           typed-peg/typing/constraint-pretty
           pprint)

  (provide (rename-out [peg-read read]
                       [peg-read-syntax read-syntax]))


  (define (peg-read in)
    (syntax->datum
     (peg-read-syntax #f in)))

  (define (show-tyvar v)
    (match v
      [(term-tyvar x)
       (string-append "t"
                      (number->string x))]))

  (define (show-type t)
    (pretty-format (ppr-type t)))

  (define (print-result res)
    (if (eq? (cdr res) 'unsat)
        (displayln "The input grammar isn't well-typed.")
        (let* ([ctx (car res)]
               [infs (cdr res)]
               [keys (map (lambda (p)
                            (cons (car p)
                                  (show-tyvar (cdr p))))
                          ctx)])
          (for ([p keys])
            (printf "~a :: ~a\n"
                    (car p)
                    (show-type (cdr (assoc (cdr p) infs))))))))

  (define (peg-read-syntax path port)
    (datum->syntax
     #f
     `(module peg-mod racket
        ,(let* ([g (parse port)]
                [result (infer g)])
           (print-result result))))))
