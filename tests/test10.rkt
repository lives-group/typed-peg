#lang typed-peg/debug/constraints-only

start: 'a' (! 'b' / 'c')*
