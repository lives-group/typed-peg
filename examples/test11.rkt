#lang typed-peg/untyped

I <-- '1' epsilon;
L <-- I I ;
W <-- epsilon I;
start: W *
