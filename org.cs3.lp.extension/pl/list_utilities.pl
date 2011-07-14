to_separated_atom([Name],_,Name) :-!.
to_separated_atom([Param|Params],Separator,Separated):-
	to_separated_atom(Params,Separator,Rest),
	atom_concat(Param,Separator,Tmp),
	atom_concat(Tmp,Rest,Separated).