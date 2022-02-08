#lang racket

(provide (all-defined-out))

;; definition of the core expressions

(struct peps
  ()
  #:transparent
  #:prefab)

(struct pchr
  (symb)
  #:transparent
  #:prefab)

(struct pvar
  (name)
  #:transparent
  #:prefab)

(struct pcat
  (left right)
  #:transparent
  #:prefab)

(struct pchoice
  (left right)
  #:transparent
  #:prefab)

(struct pneg
  (expr)
  #:transparent
  #:prefab)

(struct pstar
  (expr)
  #:transparent
  #:prefab)

(struct peg-grammar
  (rules start)
  #:transparent
  #:prefab)
