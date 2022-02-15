 #lang racket

(require typed-peg/core
         typed-peg/typing/infer 
         typed-peg/tree
         peg-gen
         rackcheck
         rackunit)

(define (peg2struct peg)
    (let ([ug (car peg)]
          [exp (cadr peg)])
    (peg-grammar (convg ug) (conve exp)) )
  )

(define (convg g)
     (if (eq? '∅ g)
         null
         (cons (cons (symbol->string (car g)) (conve (cadr g))) (convg (caddr g))))
  )

(define (conve peg)
   (match peg
         [(list '• e d)  (pcat (conve e) (conve d))]
         [(list '/ e d)  (pchoice  (conve e) (conve d) )]
         [(list '* e) (pstar  (conve e)) ]
         [(list '! e) (pneg (conve e)) ]
         ['ε (peps ) ]
         [(? natural? n)  (pchr (integer->char (+ 48 n)) )]
         [_ (pvar (symbol->string peg)) ]
         )
  )

(define straw '((I (• 1 ε) (L (• I I) (W   (• ε I) ∅) )) (* W) ((W #f (I)) (L #f (I)) (I #f ()))) )

(define (testgen p)
    (not (eq? (cdr (infer (peg2struct p))) 'unsat))
  )

(define-property type-checks([peg  (gen:peg 3 5 2)])
    (check-equal?  (testgen peg) #t)
  )

;(check-property type-checks)
