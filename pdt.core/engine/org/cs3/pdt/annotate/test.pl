/* an*/
test(Name,Comments):-
	pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,comments,Comments),
    writeln(Comments).
    
    
%x    
aja.
%y
aja.
%z
aja.
%a

%b

/* c*/
aja.