:- module(pdt_util_unification,[
	pdt_unifiable/4,
	pdt_unifiable/3,
	pdt_bound/3
]).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
pdt_unifiable(A,B,InUnifier,OutUnifier):-
    my_unifiable(A,B,InUnifier,OutUnifier).
    
pdt_unifiable(A,B,OutUnifier):-
	pdt_map_empty(InUnifier),
    my_unifiable(A,B,InUnifier,OutUnifier).

pdt_bound(Unifier,Variable,Value):-
    bound(Unifier,Variable,Value).


my_unifiable(A,B,InUnifier,OutUnifier):-
	(	source_term_var(A)
	;	source_term_var(B)
	),
	!,
	add_binding(A,B,InUnifier,OutUnifier).
	
my_unifiable(A,B,InUnifier,OutUnifier):-
	source_term_functor(A,Name,Arity),	
	source_term_functor(B,Name,Arity),
	my_unifiable_args(1,Arity,A,B,InUnifier,OutUnifier).

my_unifiable_args(N,Arity,_A,_B,Unifier,Unifier):-
    N>Arity,
    !.
my_unifiable_args(N,Arity,A,B,InUnifier,OutUnifier):-
    source_term_arg(N,A,ArgA),
    source_term_arg(N,B,ArgB),
    my_unifiable(ArgA,ArgB,InUnifier,NextUnifier),
    M is N +1,
    my_unifiable_args(M,Arity,A,B,NextUnifier,OutUnifier).
    
/*
add bindings to the unifier. 
think of the unifier as a directed, acyclic graph with the following additional 
properties:
 0 there are two kinds of nodes, variables and bound terms.
 1 for each edge (a,b) a is before b in STO.
 2 variables may have at most one outgoing edge.
 3 bound terms may not have any outgoing edges.

from 3 it follows that for each variable there is at most one well defined
path of maximum length. If the length>0, we say that the variable is bound to the node that is 
the last element of the path.

adding a binding must maintain these properties.
say we add a binding(A,B)
- If A>B in STO, recurse on binding (B,A)
- Else If A is already bound to C, unify B and C.
- Else add the edge(A,B).
   


*/    
add_binding(A,B,InUnifier,OutUnifier):-
    compare(>,A,B),
    !,
    add_binding(B,A,InUnifier,OutUnifier).
add_binding(A,B,InUnifier,OutUnifier):-
	bound(InUnifier,A,C),
	!,
	my_unifiable(B,C,InUnifier,OutUnifier).
add_binding(A,B,InUnifier,OutUnifier):-
    term_node(A,ANode),
    term_node(B,BNode),
	pdt_map_add(InUnifier, ANode,BNode,OutUnifier).
	
bound(Unifier,A,C):-
    term_node(A,Node),
    pdt_map_get(Unifier,Node,Term),
    term_node(Term,Node2),
    (	bound(Unifier,Node2,C)
    ->	true
    ;	Term=C
    ).

term_node(Var,Node):-
	source_term_var(Var),
	!,
	source_term_property(Var,variable_id,Node).    
term_node(Term,term_id(F,N)):-
	source_term_property(Term,file_ref,F),
	source_term_property(Term,n,N).    