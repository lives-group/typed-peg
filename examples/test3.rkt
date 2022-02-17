#lang typed-peg

start: ('a' 'a' 'b')* 'a' 'a' 'c' / 'a' 'a' 'd'
