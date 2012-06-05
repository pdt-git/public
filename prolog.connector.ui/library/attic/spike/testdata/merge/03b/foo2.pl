:- module(foo2,[]).
:- use_module(foo).
bar(foo2).
baz(foo2).

foo:baz(foo_Extension). %here we create an extension owned by the program that owns foo2