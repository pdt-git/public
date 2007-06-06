:- consult(prog1).
:- consult(prog2).

/*
prog1 loads a,b,c
prog2 loads d,b,e

each of the files defines a multifile predicate ad_hoc:foo/1 and adds one clause.

we use consult to cause a re-interpretation of b.pl within prog2.

the clause order resulting from merging both programms should be a,c,d,b,e. 

*/