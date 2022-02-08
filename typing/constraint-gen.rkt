#lang racket

(require typed-peg/core
         typed-peg/typing/type
         typed-peg/typing/constraint)

(provide gen-constr)

; interface for the constraint generation state

(define counter 0)

(define (get-counter)
  counter)

(define (inc-counter)
  (set! counter (add1 counter)))

(define (fresh-var)
  (let ([v (get-counter)])
    (begin
      (inc-counter)
      (term-tyvar v))))

; definition of the constraint generation.

(define (gen-constr grammar)
  (match grammar
    [(peg-grammar rs st)
     (constr-and (gen-constr-rules rs)
                 (let ([tv (fresh-var)])
                   (constr-ex tv (gen-constr-expr st tv))))]))

(define (gen-constr-rule r)
  (match r
    [(cons v e) (let* ([tv (fresh-var)]
                       [c (gen-constr-expr e tv)])
                  (constr-def v tv c))]))

(define (gen-constr-rules rs)
  (match rs
    ['() (constr-T)]
    [(cons c cs) (constr-and (gen-constr-rule c)
                             (gen-constr-rules cs))]))

(define (gen-constr-expr e ty)
  (match e
    [(peps) (constr-eq ty (type #t '()))]
    [(pchr c) (constr-eq ty (type #f '()))]
    [(pvar v) (constr-eq (pvar v)
                         ty)]
    [(pchoice e1 e2)
     (let* ([tv1 (fresh-var)]
            [tv2 (fresh-var)]
            [c1  (gen-constr-expr e1 tv1)]
            [c2  (gen-constr-expr e2 tv2)])
       (constr-ex
        tv1
        (constr-ex
         tv2
         (constr-and
          c1
          (constr-and
           c2
           (constr-eq
            ty
            (term-plus tv1 tv2)))))))]
    [(pcat e1 e2)
     (let* ([tv1 (fresh-var)]
            [tv2 (fresh-var)]
            [c1 (gen-constr-expr e1 tv1)]
            [c2 (gen-constr-expr e2 tv2)])
       (constr-ex
        tv1
        (constr-ex
         tv2
         (constr-and
          c1
          (constr-and
           c2
           (constr-eq ty
                      (term-prod tv1 tv2)))))))]
    [(pstar e1)
     (let* ([tv1 (fresh-var)]
            [c1  (gen-constr-expr e1 tv1)])
       (constr-ex
        tv1
        (constr-and c1
                    (constr-eq ty
                               (term-star tv1)))))]
    [(pneg e1)
     (let* ([tv1 (fresh-var)]
            [c1 (gen-constr-expr e1 tv1)])
       (constr-ex
        tv1
        (constr-and
         c1
         (constr-eq ty
                    (term-not tv1)))))]))
