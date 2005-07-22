:-module(lieblingsmodul,[]).
something_i_have_never_seen_before:-
    writeln('how are ya?').
:- op(50,xfx,user:lumpi).
f(Term):-
    Term=..[Name|Args].
:- f(=..(f(b(f(c))),f([f(b),f(c)])))
