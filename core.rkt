#lang racket

(provide (all-defined-out))

;; definition of the core expressions

(struct peps
  ()
  #:transparent)

(struct pchr
  (symb)
  #:transparent)

(struct pvar
  (name)
  #:transparent)

(struct pcat
  (left right)
  #:transparent)

(struct pchoice
  (left right)
  #:transparent)

(struct pneg
  (expr)
  #:transparent)

(struct pstar
  (expr)
  #:transparent)

(struct peg-grammar
  (rules start)
  #:transparent)
