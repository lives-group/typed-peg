#lang typed/racket

(require typed-peg/core
         typed-peg/tree)

(provide (rename-out [pretty peg-pretty]))

(struct error-type
  ([the-error : String])
  #:prefab)

(define-type pretty-type
  (U (Listof Char) error-type))

(: pretty (-> peg-grammar tree (U String error-type)))
(define (pretty g t)
  (match g
    [(peg-grammar rs p)
     (match (ppr rs p t)
       [(error-type err) (error-type err)]
       [str (list->string str)])]))

(: ppr-eps (-> tree pretty-type))
(define (ppr-eps t)
  (match t
    [(tunit) '()]
    [t1 (raise 'type-error)]))

(define (ppr-chr c t)
  (match t
    [(tchr c1) (if (eq? c c1)
                   (list c)
                   (raise 'type-error))]
    [t1 (raise 'type-error)]))

(define (ppr-var g v t)
  (match (assoc v g)
    [#f (begin
          (printf "Undefined variable: ~a\n~a\n" v g)
          '())]
    [(cons _ e1)
     (ppr g e1 t)]))

(define (ppr-cat g e1 e2 t)
  (match t
    [(tpair t1 t2)
     (append (ppr g e1 t1)
             (ppr g e2 t2))]
    [t1 (raise 'type-error)]))

(define (ppr-choice g e1 e2 t)
  (match t
    [(tleft t1) (ppr g e1 t1)]
    [(tright t2) (ppr g e2 t2)]
    [t1 (raise 'type-error)]))

(define (ppr-star g e t)
  (match t
    [(tlist ts)
     (append-map
      (lambda (t1) (ppr g e t1))
      ts)]
    [t1 (raise 'type-error)]))

(define (ppr-neg g e t)
  (match t
    [(tunit) '()]
    [t1 (raise 'type-error)]))

(: ppr (-> Rules p-expr tree pretty-type))
(define (ppr g e t)
  (match e
    [(peps) (ppr-eps t)]
    [(pchr c) (ppr-chr c t)]
    [(pvar v) (ppr-var g v t)]
    [(pcat e1 e2) (ppr-cat g e1 e2 t)]
    [(pchoice e1 e2) (ppr-choice g e1 e2 t)]
    [(pstar e1) (ppr-star g e1 t)]
    [(pneg e1) (ppr-neg g e1 t)]))
