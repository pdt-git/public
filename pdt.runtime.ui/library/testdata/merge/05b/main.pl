:- module(main,[]).
:- multifile bla/1.

bla(main).

:- use_module(extension).
% extension contains an ad hoc module named main.
% we own main, so we can modify it.