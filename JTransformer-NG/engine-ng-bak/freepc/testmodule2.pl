:- module(testmodule2,[test/1]).
:- consult('m:/aspectj2prolog/engine/freepc/base.pl').
%:- multifile pred1/1.
%:- dynamic pred/1.
:- use_module('m:/aspectj2prolog/engine/freepc/testmodule.pl').

%pred1(arg3).
test(_arg):-
   assert(pred1(_arg)).

pred(a).

:- assert(user:abstraction(first)).
