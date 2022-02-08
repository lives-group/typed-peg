#lang typed-peg/debug/infer-only

S <-- 'a' S 'a' / epsilon ;

start: S
