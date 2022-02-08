#lang typed/racket

(provide (all-defined-out))

;; definition of parse trees

(define-type tree
  (U tunit
     tchr
     tpair
     tleft
     tright
     tlist))

(struct tunit
  ()
  #:prefab)

(struct tchr
  ([symb : Char])
  #:prefab)

(struct tpair
  ([fst : tree]
   [snd : tree])
  #:prefab)

(struct tleft
  ([the-tree : tree])
  #:prefab)

(struct tright
  ([the-tree : tree])
  #:prefab)

(struct tlist
  ([elems : (Listof tree)])
  #:prefab)
