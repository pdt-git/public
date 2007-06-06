:- use_module(prog1).
:- use_module(prog2).

/*
prog1 loads a,b,c
prog2 loads d,b,e

each of the files defines a multifile predicate ad_hoc:foo/1 and adds one clause.

the clause order resulting from merging both programms should be a,b,c,d,e. 

*/