#lang racket

(provide (all-defined-out))

;; definition of parse trees

(struct tunit
  ()
  #:transparent
  #:prefab)

(struct tchr
  (symb)
  #:transparent
  #:prefab)

(struct tpair
  (fst snd)
  #:transparent
  #:prefab)

(struct tleft
  (tree)
  #:transparent
  #:prefab)

(struct tright
  (tree)
  #:transparent
  #:prefab)

(struct tlist
  (elems)
  #:transparent
  #:prefab)
