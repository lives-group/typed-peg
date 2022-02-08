#lang typed-peg/debug/infer-only

start: ('a' 'a' 'b')* 'a' 'a' 'c' / 'a' 'a' 'd'
