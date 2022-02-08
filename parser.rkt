#lang typed/racket

(require typed-peg/core
         typed-peg/tree)

(provide (rename-out [parse peg-parse]))

; definition of a type

(define-type string
  (Listof Char))

(struct error-type
  ([the-error : String])
  #:prefab)

(define-type parser-type
  (U (Pairof tree (Listof Char))
     error-type))

;; definition of the top level parser

(: parse (-> peg-grammar String parser-type))
(define (parse g s)
  (match g 
    [(peg-grammar rs p)
     (run-expr rs p (string->list s))]))

(: run-eps (-> string parser-type))
(define (run-eps s)
  (cons (tunit) s))

(: run-chr (-> Char string parser-type))
(define (run-chr c s)
  (match s
    ['() (error-type "Cannot parse the empty string.")]
    [(cons c1 s1)
     (if (eq? c c1)
         (cons (tchr c) s1)
         (error-type "Cannot parse the current string."))]))

(: run-var (-> Rules String string parser-type))
(define (run-var g v s)
  (define assoc1 (inst assoc String p-expr))
  (match (assoc1 v g)
    [#f (error-type (string-append "Undefined variable:" v))]
    [(cons _ e1) (run-expr g e1 s)]))

(: run-cat (-> Rules p-expr p-expr string parser-type))
(define (run-cat g e1 e2 s)
  (match (run-expr g e1 s)
    [(error-type err) (error-type err)]
    [(cons t1 s1)
     (match (run-expr g e2 s1)
       [(error-type err) (error-type err)]
       [(cons t2 s2)
        (cons (tpair t1 t2) s2)])]))

(: run-choice (-> Rules p-expr p-expr string parser-type))
(define (run-choice g e1 e2 s)
  (match (run-expr g e1 s)
    [(error-type err)
     (match (run-expr g e2 s)
       [(error-type err) (error-type err)]
       [(cons t2 s2)
        (cons (tright t2) s2)])]
    [(cons t1 s1)
     (cons (tleft t1) s1)]))

(: run-neg (-> Rules p-expr string parser-type))
(define (run-neg g e1 s)
  (match (run-expr g e1 s)
    [(error-type err) (cons (tunit) s)]
    [(cons t s1) (error-type "Negation failure.")]))

(: run-star (-> Rules p-expr string parser-type))
(define (run-star g e s)
  (match (run-expr g e s)
    [(error-type err) (cons (tlist '()) s)]
    [(cons t s1)
     (match (run-expr g (pstar e) s1)
       [(error-type err) (cons (tlist (list t)) s1)]
       [(cons (tlist t2) s2)
        (cons (tlist (cons t t2)) s2)]
       [(cons t s2) (error-type "Invalid parse tree!")])]))

(: run-expr (-> Rules p-expr string parser-type))
(define (run-expr g e s)
  (match e
    [(peps) (run-eps s)]
    [(pchr c) (run-chr c s)]
    [(pvar v) (run-var g v s)]
    [(pcat e1 e2) (run-cat g e1 e2 s)]
    [(pchoice e1 e2) (run-choice g e1 e2 s)]
    [(pneg e1) (run-neg g e1 s)]
    [(pstar e) (run-star g e s)]))
