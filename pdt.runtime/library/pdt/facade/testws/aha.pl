:- module(aha,[]).
%:- dynamic some_other_pred/0.
%:- dynamic aha:ganz_other_pred/0.
:- dynamic ja/1.
:- multifile ja/1.
somepred(X):-ja(X).
