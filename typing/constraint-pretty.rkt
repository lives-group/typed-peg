#lang racket

(require typed-peg/core
         typed-peg/typing/constraint
         typed-peg/typing/type
         pprint)

(provide ppr
         ppr-type)

(define (ppr c)
  (pretty-format (ppr-constraint c) 80))

(define (ppr-constraint c)
  (match c
    [(constr-F) (text "#f")]
    [(constr-T) (text "#t")]
    [(constr-ex tvs c)
     (hs-append (text "exists")
                (ppr-variables tvs)
                (text ".")
                (ppr-constraint c))]
    [(constr-and c1 c2)
     (hs-append (ppr-constraint c1)
                (text "&")
                (ppr-constraint c2))]
    [(constr-def nt ty c)
     (hs-append (text "def")
                (text nt)
                (text ":")
                (ppr-tyvar ty)
                (text "in")
                (ppr-constraint c))]
    [(constr-eq t1 t2)
     (hs-append (ppr-term t1)
                (text "=")
                (ppr-term t2))]
    ))

(define (ppr-term t)
  (match t
    [(term-tyvar _) (ppr-tyvar t)]
    [(term-type ty) (ppr-type ty)]
    [(term-plus t1 t2)
     (ppr-bin "+" t1 t2)]
    [(term-prod t1 t2)
     (ppr-bin "*" t1 t2)]
    [(term-star t1)
     (h-append lparen
               (ppr-term t1)
               rparen
               (text "*"))]
    [(term-not t1)
     (h-append (text "!")
               lparen
               (ppr-term t1)
               rparen)]
    [(pvar v) (text v)]
    [(type b ht) (ppr-type (type b ht))]))

(define (ppr-bin op t1 t2)
  (hs-append lparen
             (ppr-term t1)
             (text op)
             (ppr-term t2)
             rparen))

(define (ppr-type ty)
  (match ty
    [(type b hset)
     (h-append (text "<")
               (ppr-bool b)
               comma
               (ppr-hset hset)
               (text ">"))]))

(define (ppr-bool b)
  (if b (text "#t") (text "#f")))

(define (ppr-hset s)
  (h-append lbrace
            (h-concat (apply-infix comma (map text s)))
            rbrace))

(define (ppr-variables tvs)
  (if (term-tyvar? tvs)
      (ppr-tyvar tvs)
      (hs-concat (map ppr-tyvar tvs))))

(define (ppr-tyvar v)
  (match v
    [(term-tyvar n) (text (string-append "v"
                                         (number->string n)))]))



