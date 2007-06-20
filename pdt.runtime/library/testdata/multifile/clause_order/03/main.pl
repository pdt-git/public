:- ensure_loaded(prog1).
:- ensure_loaded(prog2).

/*
prog1 loads a,b,c
prog2 loads d,b,e

each of the files defines a multifile predicate ad_hoc:foo/1 and adds one clause.

we use ensure_loaded/1. In both programs, b.pl will be consulted in module user, so in prog2, it should not be
re-interpreted.

the clause order resulting from merging both programms should be a,b,c,d,e. 

*/