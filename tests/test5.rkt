#lang typed-peg/debug/infer-only

A <-- 'a' A 'b' / epsilon;

start: A
