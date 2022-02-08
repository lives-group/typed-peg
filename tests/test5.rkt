#lang typed-peg

A <-- 'a' A 'b' / epsilon;

start: A
