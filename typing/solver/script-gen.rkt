#lang racket

(require typed-peg/core
         typed-peg/typing/constraint
         typed-peg/typing/constraint-pretty
         typed-peg/typing/type)

(provide create-smt-script
         smt-script-string)

;; top level script creation function

(define (create-smt-script ctx vars eq [path #f])
  (let* ([script-file (if path
                         (string->path path)
                         (make-temporary-file "constr-~a.z3"))]
         [script-text (smt-script-string ctx vars eq)]
         [out (open-output-file script-file
                                #:mode 'text
                                #:exists 'truncate)])
    (begin
      (displayln script-text out)
      (close-output-port out)
      script-file)))

(define (smt-script-string ctx vars eqs)
  (let ([content (string-append
                     (create-nt-type ctx)
                     "\n"
                     script-fixed-stuff
                     "\n"
                     (create-vars (set-union (map cdr ctx) vars))
                     "\n"
                     (create-assertions ctx eqs)
                     "\n"
                     footer)])
    content))

;; data type creation for non terminals

(define (create-nt-type ctx)
  (string-join (map car (if (null? ctx)
                            ;; dirty hack to ensure existence of NT type
                            (list (cons (string-append "Noob"
                                                       (number->string (random 1000)))
                                        '())) 
                            ctx))
               " "
               #:before-first "(declare-datatypes () ((NT "
               #:after-last ")))"))

;; defining the definition of type variables

(define (create-vars vars)
  (define (declare-var v)
    (match v
      [(term-tyvar n) (string-append "(declare-const t"
                                     (number->string n)
                                     " Type)")]))
  (string-join (map declare-var vars) "\n"))

;; definition of assertions

(define (create-assertions ctx eqs)
  (string-join (map (lambda (e) (create-assertion ctx e))
                    eqs)
               "\n"))

;; converting a variable (number) to an identifier 

(define (var->string v)
  (string-append "t" (number->string v)))

;; generating a string for building head-set

(define (head-set-string ht)
  (define (hss ht)
    (match ht
      ['() (cons "empty" "")]
      [(cons h hts) (let ([p (hss hts)])
                      (cons (string-append "(union "
                                           h
                                           " "
                                           (car p))
                            (string-append ")"
                                           (cdr p))))]))
  (let ([p (hss ht)])
    (string-append (car p) (cdr p))))

;; creating a string for type constants

(define (mk-type-string nt ht)
  (define b-str (if nt "true" "false"))
  (string-append "(mk-type "
                 b-str
                 " "
                 (head-set-string ht)
                 ")"))

; simple function to add a comment string
; with the original constraint pretty-printed.

(define (with-trace-info eq s)
  (string-append ";"
                 (ppr eq)
                 "\n"
                 s))

(define (create-assertion ctx eq)
  (match eq
    [(constr-eq (term-tyvar t1) (term-tyvar t2))
     (with-trace-info eq (tyvar-assertion-eq t1 t2))]
    [(constr-eq (term-tyvar t1) (type nt ht))
     (with-trace-info eq (type-assertion-eq t1 nt ht))]
    [(constr-eq (term-tyvar t1) (term-star (term-tyvar t2)))
     (with-trace-info eq (star-assertion-eq t1 t2))]
    [(constr-eq (term-tyvar t1) (term-prod (term-tyvar t2) (term-tyvar t3)))
     (with-trace-info eq (prod-assertion-eq t1 t2 t3))]
    [(constr-eq (term-tyvar t1) (term-plus (term-tyvar t2) (term-tyvar t3)))
     (with-trace-info eq (plus-assertion-eq t1 t2 t3))]
    [(constr-eq (term-tyvar t1) (term-not (term-tyvar t2)))
     (with-trace-info eq (not-assertion-eq t1 t2))]
    [(constr-eq (pvar v) (term-tyvar t1))
     (with-trace-info eq (nt-assertion-eq ctx v t1))]))

(define (nt-assertion-eq ctx v t1)
  (let ([t2 (assoc v ctx)])
    (if t2
        (member-assertion v (cdr t2) t1)
        (error (string-append "Undefined variable:" v)))))

(define (member-assertion v t2 t1)
  (let* ([v1 (var->string t1)]
         [v2 (var->string (term-tyvar-tyvar t2))])
    (string-append "(assert (not (member "
                   v
                   " "
                   v2
                   ")))"
                   "\n"
                   "(assert (= "
                   v1
                   " (mk-type (is-null "
                   v2
                   ") (union (singleton "
                   v
                   ") (head-set "
                   v2
                   ")))))")))

(define (plus-assertion-eq t1 t2 t3)
  (bin-assertion-eq "(sum" t1 t2 t3))

(define (prod-assertion-eq t1 t2 t3)
  (bin-assertion-eq "(prod" t1 t2 t3))

(define (bin-assertion-eq s t1 t2 t3)
  (string-join (list (var->string t1)
                     s
                     (var->string t2)
                     (var->string t3))
               " "
               #:before-first "(assert (= "
               #:after-last ")))"))


(define (tyvar-assertion-eq t1 t2)
  (string-join  (list (var->string t1)
                      (var->string t2))
                " "
                #:before-first "(assert (= "
                #:after-last "))"))

(define (type-assertion-eq t1 nt ht)
  (string-join (list (var->string t1)
                     (mk-type-string nt ht))
               " "
               #:before-first "(assert (= "
               #:after-last "))"))

(define (not-assertion-eq t1 t2)
  (unary-assertion-eq "(neg" t1 t2))

(define (star-assertion-eq t1 t2)
  (string-append
      (unary-assertion-eq "(star" t1 t2)
      "\n"
      (string-join (list (var->string t2))
                   #:before-first "(assert (not (is-null "
                   #:after-last ")))")))

(define (unary-assertion-eq s t1 t2)
  (string-join (list (var->string t1)
                     s
                     (var->string t2))
               " "
               #:before-first "(assert (= "
               #:after-last ")))"))

;; fixed stuff of the script

(define footer
  "(check-sat)\n(get-model)")

(define script-fixed-stuff
  "\n
(define-fun empty () (Set NT)
  ((as const (Set NT)) false))

(define-fun singleton ((a NT)) (Set NT)
  (store empty a true))

(define-fun union ((a (Set NT)) (b (Set NT))) (Set NT)
  ((_ map or) a b))

(define-fun imp ((b Bool) (s (Set NT))) (Set NT)
  (ite b s empty))

(declare-datatypes () ((Type (mk-type (is-null Bool) (head-set (Set NT))))))

(define-fun prod ((a Type) (b Type)) (Type)
  (mk-type (and (is-null a) (is-null b))
           (union (head-set a) (imp (is-null a) (head-set b)))))

(define-fun sum ((a Type) (b Type)) (Type)
  (mk-type (or (is-null a)
               (is-null b))
           (union (head-set a)
                  (head-set b))))

(define-fun star ((a Type)) (Type)
  (mk-type true
           (head-set a)))

(define-fun neg ((a Type)) (Type)
  (mk-type true
           (head-set a)))

(define-fun member ((a NT) (t Type)) (Bool)
  (select (head-set t) a))")

