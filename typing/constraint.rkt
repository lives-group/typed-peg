#lang racket

(provide (all-defined-out))

; constraint syntax

(struct constr-F
  ()
  #:transparent)

(struct constr-T
  ()
  #:transparent)

(struct constr-and
  (left right)
  #:transparent)

(struct constr-eq
  (left right)
  #:transparent)

(struct constr-ex
  (tyvar constr)
  #:transparent)

(struct constr-def
  (non-term ty constr)
  #:transparent)

; term syntax

(struct term-type
  (type)
  #:transparent)

(struct term-tyvar
  (tyvar)
  #:transparent)

(struct term-plus
  (left right)
  #:transparent)

(struct term-prod
  (left right)
  #:transparent)

(struct term-not
  (type)
  #:transparent)

(struct term-star
  (type)
  #:transparent)
