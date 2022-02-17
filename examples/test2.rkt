#lang typed-peg

S <-- 'a' S 'a' / epsilon ;

start: S
