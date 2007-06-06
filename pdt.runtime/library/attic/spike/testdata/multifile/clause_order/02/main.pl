:- ensure_loaded(prog1).
:- ensure_loaded(prog2).

/*
prog1 loads a,b,c
prog2 loads d,b,e

each of the files defines a multifile predicate ad_hoc:foo/1 and adds one clause.

we use ensure_loaded/1. Both programs do however define different modules. So b.pl is reinterpreted in the 
context of prog2.

the clause order resulting from merging both programms should be a,c,d,b,e. 

*/