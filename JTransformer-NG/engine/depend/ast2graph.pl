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
*     Arg1 is the node type (functor) of arg2 and arg3 is its identity.
*     If arg1 is a variable the predicate enumerates all the nodes
*     including their negation (eg. 'a' and 'not(a)').
* 
* Pseudo Knoten: implementsT, extendsT, modifierT,
* 
*****************************************************************/

ast_node(Functor, Term, ID) :-
    tree_kind(Functor,Len),
%       (var(Term)->tree_kind(Functor,Len);true),
    functor(Term, Functor, Len),
    Term =.. [Functor,ID|_],
    check_tree_existence(Term,Functor,Len).   
%   <--- GK: Wofuer dieser check_tree_existence Test? 
%   Er wiederholt doch nur den "tree_kind" Aufruf,
%   der hier schon ganz am Anfang stand... 

ast_node(_type, Term, _id) :- 
    nonvar(Term),
    Term = not(_tree),
        !,
        ast_node(_type, _tree, _id).


/************************* edges ***************************
*  ast_edge(?type,+term,?edge)
*****************************************************************/

ast_edge(EdgeName, Term, Edge):-
        ast_argument(id,EdgeName, Term, Edge).

ast_edge(_type, Term, _id) :-
    nonvar(Term),
    Term = not(_tree),
        !,
        ast_edge(_type, _tree, _id).
/*
ast_edge(_type, not(_tree), _eid) :-
    !,
    ast_edge(_type, _tree, _eid).
*/

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
        % treeSignature(Kind,Len).   <--- GK: deprecated, java-specific representation
        ast_node_def(_Lang,Kind,ArgList),   % <-- replacement for treeSignature
        length(ArgList,Len).                % <-- replacement for treeSignature
tree_kind(Kind,Len):- 
        attribSignature(Kind, Len).

/**
 *  ast_argument(ArgKind,EdgeName, Term, Edge)
 *    
 */
ast_argument(ArgKind,EdgeName, Term, Edge) :-
%       (var(Term)->tree_kind(Functor,Len);true),
        tree_kind(Functor,Len),
    functor(Term, Functor, Len),
%   check_tree_existence(Term,Functor,Len),
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

/* GK: Refactored, neue version s.u.
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
*/              

/**
 * Throw an exception if tree_kind(arg2,arg3) is false. 
 */

check_tree_existence(_Term, Kind, Len):-
       tree_kind(Kind,Len),
       !.
check_tree_existence(Term, _Kind,_Len):-
      prolog_current_frame(Frame),
          stack_for_frame_atom(Frame,Trace),
          sformat(Msg, 'not an AST Node: ~w~n~w',[Term,Trace]),
          write(Msg),
          throw(illegal_argument_exeception(Msg)).

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


