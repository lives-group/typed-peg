#lang scribble/manual
@require[@for-label[typed-peg
                    racket/base]]

@title{typed-peg: A parsing library for parsing expression grammars}
@author{Elton Cardoso, Leonardo Reis, Rodrigo Ribeiro}

@defmodule[peg]

@section{Introduction}

This package defines a racket language for parsing expression grammars that
creates a parse function for this grammar and it returns a parse tree for the input string.
The language also provide a type directed semantics for pretty printing them. 
The library also uses a type inference algorithm which determines if the PEG is complete,
i.e. terminates its execution on all inputs.

@section{Requirements}

In order to type check, the tool need a working installation of 
[Z3 SMT Solver](https://github.com/Z3Prover/z3). The project is known to work with 
Z3 version 4.8.14.

@section{Languages}

Following the racket approach to build small languages, we have build some auxiliar languages 
to ease the task of use/debug the tool.

@itemlist[#:style 'ordered
          @item{peg: default language, provides a parse and pretty printing function for the
                specified PEG, after infering types for the input PEG.}
          @item{peg/untyped: disable the type-inference engine. Use at your own risk!}
          @item{peg/debug/tokenize-only: outputs the result of the lexical analyser.}
          @item{peg/debug/parse-only: outputs the result of the parser.}
          @item{peg/debug/constraints-only: outputs the constraints generated by the algorithm.}
          @item{peg/debug/z3-script-only: outputs the z3 script that encode the constraints.}
          @item{peg/debug/infer-only: outputs the infered types for each grammar non-terminal.}]
