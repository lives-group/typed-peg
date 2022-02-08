#lang typed/racket

(provide (all-defined-out))

;; definition of the core expressions

(define-type p-expr
  (U peps
     pchr
     pany
     pvar
     pcat
     pchoice
     pneg
     pstar))

(struct peps
  ()
  #:prefab)

(struct pchr
  ([symb : Char])
  #:prefab)

(struct pany
  ()
  #:prefab)

(struct pvar
  ([name : String])
  #:prefab)

(struct pcat
  ([left  : p-expr]
   [right : p-expr])
  #:prefab)

(struct pchoice
  ([left  : p-expr]
   [right : p-expr])
  #:prefab)

(struct pneg
  ([expr : p-expr])
  #:prefab)

(struct pstar
  ([expr : p-expr])
  #:prefab)

(define-type Rules
  (Listof (Pairof String p-expr)))

(struct peg-grammar
  ([rules : Rules ]
   [start : p-expr])
  #:prefab)
