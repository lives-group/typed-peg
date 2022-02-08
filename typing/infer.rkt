#lang racket

;; main type inference driver

(require typed-peg/core
         typed-peg/typing/type
         typed-peg/typing/constraint-gen
         typed-peg/typing/constraint-solver
         typed-peg/typing/solver/model-parser)

(provide infer)

(define (infer g)
  (let* ([constr (gen-constr g)]
         [p (solver constr)]
         [result-string (cdr p)])
        (cons (car p) (parse (open-input-string result-string)))))
