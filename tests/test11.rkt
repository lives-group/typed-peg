#lang typed-peg/debug/infer-only

I <-- '1' epsilon;
L <-- I I ;
W <-- epsilon I;
start: W *
