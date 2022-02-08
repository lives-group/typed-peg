#lang racket

(provide (all-defined-out))

; constraint syntax

(struct constr-F
  ()
  #:transparent
  #:prefab)

(struct constr-T
  ()
  #:transparent
  #:prefab)

(struct constr-and
  (left right)
  #:transparent
  #:prefab)

(struct constr-eq
  (left right)
  #:transparent
  #:prefab)

(struct constr-ex
  (tyvar constr)
  #:transparent
  #:prefab)

(struct constr-def
  (non-term ty constr)
  #:transparent
  #:prefab)

; term syntax

(struct term-type
  (type)
  #:transparent
  #:prefab)

(struct term-tyvar
  (tyvar)
  #:transparent
  #:prefab)

(struct term-plus
  (left right)
  #:transparent
  #:prefab)

(struct term-prod
  (left right)
  #:transparent
  #:prefab)

(struct term-not
  (type)
  #:transparent
  #:prefab)

(struct term-star
  (type)
  #:transparent
  #:prefab)
