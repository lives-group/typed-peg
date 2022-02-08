#lang racket

(provide (all-defined-out))

;; definition of parse trees

(struct tunit
  ()
  #:transparent)

(struct tchr
  (symb)
  #:transparent)

(struct tpair
  (fst snd)
  #:transparent)

(struct tleft
  (tree)
  #:transparent)

(struct tright
  (tree)
  #:transparent)

(struct tlist
  (elems)
  #:transparent)
