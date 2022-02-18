#lang info
(define collection "typed-peg")
(define deps '("base"
               "pprint"
               "peg-gen"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))
(define scribblings '(("scribblings/typed-peg.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(rodrigo))
(define license '(Apache-2.0 OR MIT))
