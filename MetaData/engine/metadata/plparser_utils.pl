:- module(plparser_utils,
	[	contains/2,
		in_toplevel/2,
		in_compilation_unit/2,
		variable_name/2
	]).
:- use_module(plparser).
contains(P,P).
contains(P,Q):-
    ground(P),
    !,
    node_attr(P,child(C)),
    contains(C,Q).
contains(P,Q):-
    ground(Q),
    !,
    node_attr(Q,parent(C)),
    contains(P,C).

in_toplevel(Node,TLT):-
	contains(TLT,Node),
	node_attr(TLT,toplevel_term).	    
	
in_compilation_unit(Node,compilation_unit_node(C)):-
	contains(compilation_unit_node(C),Node).

variable_name(variable_node(V),Name):-
    in_toplevel(variable_node(V),T),
    node_attr(T,variable_names(VNames)),
    node_attr(variable_node(V),term(Variable)),
    member(Name=Variable,VNames).


	