#lang typed-peg/debug/infer-only

S <-- !! A 'a'* B ! C ;
B <-- 'b' B 'c' / epsilon ;
A <-- 'a' A 'b' / epsilon ;
C <-- 'a' / 'b' / 'c' ;
start: S
