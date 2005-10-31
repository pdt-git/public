:- module(plast_utils,
	[	contains/2,
		in_toplevel/2,
		in_compilation_unit/2,
		variable_symbol/3,
		toplevel_term/5
	]).
	
:- use_module(plast).
contains(P,P).
contains(P,Q):-
    ground(P),
    !,
    plast_prop(P,child(C)),
    contains(C,Q).
contains(P,Q):-
    ground(Q),
    !,
    plast_prop(Q,parent(C)),
    contains(P,C).

in_toplevel(Node,TLT):-
	contains(TLT,Node),
	plast_prop(TLT,toplevel_term).	    
	
in_compilation_unit(Node,compilation_unit_node(C)):-
	contains(compilation_unit_node(C),Node).

variable_symbol(variable_node(V),Frame,varsym(T,Frame,Term)):-
    in_toplevel(variable_node(V),T),
    plast_prop(variable_node(V),term(Term)).

%operator_type(compound_node(V),prefix):-
%    plast_prop(compound_node(V),functor(F/1)),
%    in_toplevel(compound_node(V),TL),
%    plast_prop(TL,module(Module)),    
%    plast_prop(compound_node(V),functor_position(From-_)),
%    plast_prop(compound_node(V),From-_).


toplevel_term(Id,Type,Module,Head,Body):-
    plast_prop(Id,toplevel_term),
    do_toplevel_term(Id,Type,M,Head,Body),
    (	M==[]
    ->	plast_prop(Id,module(Module))
    ;	Module=M
    ).
    
do_toplevel_term(Id,directive,[],[],Body):-
    plast_prop(Id,functor((:-)/1)),
    plast_prop(Id,arguments([Body])).
do_toplevel_term(Id,rule,Module,Head,Body):-
    plast_prop(Id,functor((:-)/2)),
    plast_prop(Id,arguments([Left,Body])),
    (	plast_prop(Left,functor((:)/2))
    ->	plast_prop(Left,arguments([Module,Head]))	
    ;	Module=[],
	   	Head=Left
    ).
do_toplevel_term(Id,Type,Module,Head,Body):-
    plast_prop(Id,functor((:)/2)),
    plast_prop(Id,arguments([Module,TL])),
    do_toplevel_term(TL,Type,[],Head,Body).
do_toplevel_term(Id,fact,Module,Head,[]):-
    (	plast_prop(Id,functor((:)/2))
    ->	plast_prop(Id,arguments([Module,Head]))	
    ;	Module=[],
    	Head=Id
    ).


	