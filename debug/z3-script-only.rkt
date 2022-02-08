#lang racket

(module reader racket

  (require typed-peg/grammar
           typed-peg/typing/constraint-gen
           typed-peg/typing/constraint-pretty
           typed-peg/typing/constraint-solver
           typed-peg/typing/solver/script-gen)

  (provide (rename-out [peg-read read]
                       [peg-read-syntax read-syntax]))


  (define (peg-read in)
    (syntax->datum
     (peg-read-syntax #f in)))

  (define (peg-read-syntax path port)
    (datum->syntax
     #f
     `(module peg-mod racket
        ,(displayln
          (let* ([g (parse port)]
                 [c (gen-constr g)]
                 [c1 (gen-context c)]
                 [c2 (move-exists (cdr c1))]
                 [eqs (group-equalities c2)])
            (smt-script-string (car c1)
                               (car eqs)
                               (cdr eqs)))))))

  )
