#lang racket

(require racket/set)

(provide (all-defined-out))

(struct type
  (null? head-set)
  #:prefab)

; operations on types

(define (ty-product t1 t2)
  (let ([b (type-null? t1)])
    (type (and b (type-null? t2))
          (set-union (type-head-set t1)
                     (if (type-null? t1)
                         (type-head-set t2)
                         '())))))

(define (ty-sum t1 t2)
  (type (or (type-null? t1)
            (type-null? t2))
        (set-union (type-head-set t1)
                   (type-head-set t2))))

(define (ty-not t)
  (type #t (type-head-set t)))

(define (ty-star t)
  (if (type-null? t)
      #f
      (type #t (type-head-set t))))
