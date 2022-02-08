#lang racket

(provide (all-defined-out))

;; definition of parse trees

(struct tunit
  ()
  #:prefab)

(struct tchr
  (symb)
  #:prefab)

(struct tpair
  (fst snd)
  #:prefab)

(struct tleft
  (tree)
  #:prefab)

(struct tright
  (tree)
  #:prefab)

(struct tlist
  (elems)
  #:prefab)
