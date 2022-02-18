#lang typed-peg/untyped

start: ('a' 'a' 'b')* 'a' 'a' 'c' / 'a' 'a' 'd'
