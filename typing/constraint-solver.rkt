#lang racket

(require typed-peg/core
         typed-peg/typing/type
         typed-peg/typing/constraint
         typed-peg/typing/solver/script-gen)

(provide solver
         gen-context
         move-exists
         group-equalities)

;; solver interface

(define (solver c [path #f])
  (let* ([c1 (gen-context c)]
         [ctx (car c1)]
         [c11 (cdr c1)]
         [c2 (move-exists c11)]
         [p (group-equalities c2)]
         [script-file (create-smt-script ctx (car p) (cdr p) path)]
         [cmd (string-append "z3 " (path->string script-file))]
         [res (with-output-to-string (lambda () (system cmd)))])
    (cons ctx res)))


;; constraint solving step 1: generating context,
;; removing the definition constraint.

(define (gen-context c)
  (match c
    [(constr-F) (cons '() (constr-F))]
    [(constr-T) (cons '() (constr-T))]
    [(constr-and c1 c2)
     (let* ([r1 (gen-context c1)]
            [r2 (gen-context c2)])
       (cons (append (car r1)
                     (car r2))
             (constr-and (cdr r1)
                         (cdr r2))))]
    [(constr-eq ty1 ty2)
     (cons '() (constr-eq ty1 ty2))]
    [(constr-ex v c1) (let ([r1 (gen-context c1)])
                        (cons (car r1)
                              (constr-ex v (cdr r1))))]
    [(constr-def v ty c1)
     (let* ([r1 (gen-context c1)]
            [g (cons (cons v ty) (car r1))]
            [c2 (cdr r1)])
       (cons g c2))]))

;; constraint solving step 2: moving existential quantification.

(define (elim-exists c)
  (match c
    [(constr-F) (cons '() (constr-F))]
    [(constr-T) (cons '() (constr-T))]
    [(constr-and c1 c2)
     (let* ([r1 (elim-exists c1)]
            [r2 (elim-exists c2)]
            [xs (append (car r1)
                        (car r2))])
       (cons xs (constr-and (cdr r1)
                            (cdr r2))))]
    [(constr-eq ty1 ty2)
     (cons '() (constr-eq ty1 ty2))]
    [(constr-ex v c)
     (let* ([r1 (elim-exists c)]
            [xs (cons v (car r1))]
            [c1 (cdr r1)])
       (cons xs c1))]
    ))

(define (move-exists c)
  (let* ([r (elim-exists c)]
         [c1 (cdr r)]
         (tvs (car r)))
    (constr-ex tvs c1)))


;; group equalities

(define (group-equalities c)
  (match c
    [(constr-ex tvs c) (cons tvs (group c))]))

(define (group c)
  (match c
    [(constr-and c1 c2) (append (group c1)
                                (group c2))]
    [(constr-ex _ _) '()]
    [(constr-T) '()]
    [(constr-F) '()]
    [(constr-eq ty1 ty2) (list (constr-eq ty1 ty2))]))
