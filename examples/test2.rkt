#lang typed-peg/untyped

S <-- 'a' S 'a' / epsilon ;

start: S
