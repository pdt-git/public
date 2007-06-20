:- module(foo,[]).
:- use_module(main).
bar(foo).
baz(foo).

main:baz(foo_Extension). %here we create an extension owned by the program that owns main