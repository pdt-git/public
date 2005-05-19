:- multifile ast_node/3.
:- multifile ast_edge/3.
:- multifile ast_attr/3.

/*****************************************************************
 * Graph Sicht auf Programmelemente (unabhängig von Faktenbasis)
 *  ast_node(?type, +term, ?id)
 *  ast_edge(?type, +term, ?eid)
 *  ast_attr(?type, +term, ?attr)
 *****************************************************************/

/************************* nodes ***************************
*  ast_node(?type, +term, ?id)
*
* Pseudo Knoten: implementsT, extendsT, modifierT,
* 
*****************************************************************/

ast_node(Functor, Term, ID) :-
    tree_kind(Functor,Len),
%	(var(Term)->tree_kind(Functor,Len);true),
    functor(Term, Functor, Len),
    Term =.. [Functor,ID|_],
    check_tree_existence(Term,Functor,Len).

ast_node(_type, not(_tree), _id) :- 
	!,
	ast_node(_type, _tree, _id).


/************************* edges ***************************
*  ast_edge(?type,+term,?edge)
*****************************************************************/

ast_edge(EdgeName, Term, Edge):-
	ast_argument(id,EdgeName, Term, Edge).

ast_edge(_type, not(_tree), _eid) :- 
    !,
    ast_edge(_type, _tree, _eid).

/************************* attributes ***************************
*  ast_attr(?type,+term,?attr)
*****************************************************************/

ast_attr(EdgeName, Term, Edge):-
	ast_argument(attr,EdgeName, Term, Edge).

ast_attr(_type, not(_tree), _eid) :- 
    !,
    ast_attr(_type, _tree, _eid).

/******************************************
 *      helper predicates                 *
 ******************************************/

tree_kind(Kind,Len):- 
	treeSignature(Kind,Len).
tree_kind(Kind,Len):- 
	attribSignature(Kind, Len).

ast_argument(ArgKind,EdgeName, Term, Edge) :-
%	(var(Term)->tree_kind(Functor,Len);true),
	tree_kind(Functor,Len),
    functor(Term, Functor, Len),
    %check_tree_existence(Term,Functor,Len),
%   treeSignature(Kind, Len),
    Term =.. [Functor,_ID|Args],
    ast_node_def(_,Functor,[_|ArgSpec]),
    valid_ast_argument(ArgKind,Args,ArgSpec,EdgePairList),
    member([EdgeName,Edge],EdgePairList).

/**
 * valid_ast_argument(Kind, Edges, Specs, [[EdgeName,Edge]|..])
 *
 * Kind = attr or id
 * bind the third argument to a list of all edge name, 
 * edge pairs.
 */

valid_ast_argument(_ArgKind,[],_,[]).

valid_ast_argument(ArgKind,[Edge|Edges],
               [ast_arg(EdgeName,mult(_0or1,1,no),ArgKind,_)|Specs],
               [[EdgeName,Edge]|EdgePair]):-
    !,
	valid_ast_argument(ArgKind,Edges,Specs,EdgePair).

% retrieves a list of 
valid_ast_argument(ArgKind,[List|Edges],
               [ast_arg(EdgeName,mult(0,*,ord),ArgKind,_)|Specs],
               EdgeListAppended):-
    is_list(List),
    !,
    maplist(prepend_atom(EdgeName),List,EdgePairs),
	valid_ast_argument(ArgKind, Edges,Specs,EdgePairRest),
	append(EdgePairs,EdgePairRest,EdgeListAppended).
    
valid_ast_argument(ArgKind,[_|Edges],
               [_|Specs],
               EdgePair):-
	valid_ast_argument(ArgKind,Edges,Specs,EdgePair).
    
prepend_atom(EdgeName,H,[EdgeName,H]).

check_tree_existence(Term, Kind,Len):-
       not(tree_kind(Kind,Len))->
	(
	  prolog_current_frame(Frame),
	  stack_for_frame_atom(Frame,Trace),
	  sformat(Msg, 'not an AST Node: ~w~n~w',[Term,Trace]),
	  write(Msg),
	  throw(illegal_argument_exeception(Msg))
	);
		true.

/*
my_ast_node(Kind, Term, ID) :-
    (
      nonvar(Kind) -> (
	    treeSignature(Kind, Len),
	    functor(Term, Kind, Len)
	    );(
	    functor(Term, Kind, Len),
	    treeSignature(Kind, Len)
	    )
	),  
    Term =.. [Kind,ID|_].
*/


