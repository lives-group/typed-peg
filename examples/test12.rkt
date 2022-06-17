#lang typed-peg

K <-- (epsilon '3') / (epsilon '2');
C <-- ('2' / E) (! K);
E <-- ('1' / '3') ('3' / C);
start: (epsilon C) *
