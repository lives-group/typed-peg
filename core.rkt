#lang racket

(provide (all-defined-out))

;; definition of the core expressions

(struct peps
  ()
  #:prefab)

(struct pchr
  (symb)
  #:prefab)

(struct pvar
  (name)
  #:prefab)

(struct pcat
  (left right)
  #:prefab)

(struct pchoice
  (left right)
  #:prefab)

(struct pneg
  (expr)
  #:prefab)

(struct pstar
  (expr)
  #:prefab)

(struct peg-grammar
  (rules start)
  #:prefab)
