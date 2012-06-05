:- module(foo,[bar/0, bang/1]).

:- multifile bang/1.

/*
 interesting variant: if the predicate is not declared multifile here, but only in the
 outermost program, we get a problem.
 
 */

bar.
baz.
bang(foo).