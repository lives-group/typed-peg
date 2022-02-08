#lang typed-peg/untyped

start: 'a' (! 'b' / 'c')*
