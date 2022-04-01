#lang racket

(require typed-peg/core
         typed-peg/tree)

(provide (rename-out [parse peg-parse]))

;; definition of the top level parser

(define (parse g s)
  (match g
    [(peg-grammar rs p)
     (let* ([inp (string->list s)]
            [r (run-expr rs p inp)])
       (if (null? r)
           (displayln "Could not parse the input string!")
           (car r)))]))

(define (run-eps s)
  (cons (tunit) s))

(define (run-chr c s)
  (match s
    ['() '()]
    [(cons c1 s1)
     (if (eq? c c1)
         (cons (tchr c) s1)
         '())]))

(define (run-any s)
  (match s
    ['() '()]
    [(cons c s1) (cons (tchr c) s1)]))

(define (run-var g v s)
  (match (assoc v g)
    [#f (begin
          (printf "Undefined variable: ~a\n~a\n" v g)
          '())]
    [(cons _ e1) (run-expr g e1 s)]))

(define (run-cat g e1 e2 s)
  (match (run-expr g e1 s)
    ['() '()]
    [(cons t1 s1)
     (match (run-expr g e2 s1)
       ['() '()]
       [(cons t2 s2)
        (cons (tpair t1 t2) s2)])]))

(define (run-choice g e1 e2 s)
  (match (run-expr g e1 s)
    ['() (match (run-expr g e2 s)
           ['() '()]
           [(cons t2 s2)
            (cons (tright t2) s2)])]
    [(cons t1 s1)
     (cons (tleft t1) s1)]))

(define (run-neg g e1 s)
  (match (run-expr g e1 s)
    ['() (cons (tunit) s)]
    [(cons t s1) '()]))

(define (run-star g e s)
  (match (run-expr g e s)
    ['() (cons (tlist '()) s)]
    [(cons t s1)
     (match (run-expr g (pstar e) s1)
       ['() (cons (tlist (list t)) s1)]
       [(cons (tlist t2) s2)
        (cons (tlist (cons t t2)) s2)]
       [(cons t s2) (raise 'invalid-tree)])]))

(define (run-expr g e s)
  (match e
    [(peps) (run-eps s)]
    [(pchr c) (run-chr c s)]
    [(pany) (run-any s)]
    [(pvar v) (run-var g v s)]
    [(pcat e1 e2) (run-cat g e1 e2 s)]
    [(pchoice e1 e2) (run-choice g e1 e2 s)]
    [(pneg e1) (run-neg g e1 s)]
    [(pstar e) (run-star g e s)]))
