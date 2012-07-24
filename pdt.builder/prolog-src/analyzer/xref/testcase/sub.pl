/* $LICENSE_MSG$ */

:- module(sub,[]).

:- use_module(super).

a(X) :- b(X), c(X), X, super:d(X), other:e(X).

call_e(X) :- other:e(X).

b(1).
%b(2).

d(1).
d(2).

