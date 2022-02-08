#lang typed-peg/debug/infer-only

X2 <-- epsilon '2' / '2' X0 epsilon ; 
X1 <-- '2' / X2 / '3' / X2 ;
X0 <-- '2' * / '3' / X1 ;

start: '0' * / epsilon / X0
