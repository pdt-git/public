:- module(testmodule,[pred1/1]).
:- consult('m:/aspectj2prolog/engine/freepc/base.pl').
:- multifile pred1/1.
:- dynamic pred1/1.

pred1(arg1).
pred1(arg2).

pred(a).


%:- assert(user:abstraction(second)).
user:abstraction(second).
