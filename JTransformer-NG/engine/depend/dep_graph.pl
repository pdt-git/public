% Author:
% Date: 04.11.02
%
% gen_dependency_graph(+filename, +[ct's]).
%

/*
:- module(dep_graph,
          [ gen_dependency_graph/2
          ]).
*/

% cache computation results
:- multifile posDepend/3.
:- multifile negDepend/3.

:- dynamic use_cache_results_only/0.
:- dynamic ct_node/1.
:- dynamic ct_edge/4.
:- dynamic ct_order/1.
:- dynamic ct_conflict/1.
:- dynamic ct_circle/1.

dep_graph(_l) :-
    gen_dep_graph(_l),
    del_dep_graph.
    
gen_dep_graph([]).
gen_dep_graph(_ctlist) :-
    current_output(_filestream),
    !,
    gen_dependency_graph(_filestream, _ctlist),
    !.
gen_dep_graph(_, []).
gen_dep_graph(_filename, _ctlist) :-
    open(_filename, write, _filestream),
    !,
    gen_dependency_graph(_filestream, _ctlist),
    !,
    close(_filestream).

gen_dependency_graph(_filestream, _ctlist) :-
    % assert ct_node facts
    findall(_ct, (member(_ct, _ctlist), ct(_ct,_,_), gen_node(_filestream, _ct)), _ctl1),
    format(_filestream,'~n',[]),

    % assert's edge facts
    findall((_ct1,_ct2), (member(_ct1, _ctlist), member(_ct2, _ctlist), posDepend(_ct1, _ct2, _label), gen_edge(_filestream, _ct1, _ct2, _label, positive)), _ctl2),
    length(_ctl2, _length2),
    format(_filestream,'found ~a positive dependencies:~n',[_length2]),
    findall((_ct3,_ct4), (member(_ct3, _ctlist), member(_ct4, _ctlist), negDepend(_ct3, _ct4, _label), gen_edge(_filestream, _ct3, _ct4, _label, negative)), _ctl3),
    length(_ctl3, _length3),
    format(_filestream,'found ~a negative dependencies:~n',[_length3]),
    format(_filestream,'~n',[]),

    % compute order or report conflict and circles
    assert(use_cache_results_only),
    
% FIX: maplist auskommentiert, da sonst die Listenelemente 
% mit Hochkommata eingeschlossen werden, was dazu führt, dass apply_ct fehlschlägt
%    maplist(term_to_atom, _ctlist, _ctlist2),

    gen_order(_filestream, _ctlist),
    
%    format('~nNach Analyse del_dep_graph/0 aufrufen !~n~n',[]),
    !.
    
del_dep_graph :-
    % clear cache
    retractall(use_cache_results_only),
    retractall(ct_node(_)),
    retractall(ct_edge(_,_,_,_)),
    retractall(ct_order(_)),
    retractall(ct_conflict(_)),
    retractall(ct_circle(_)),
    format('~ndel_dep_graph/0 wurde aufrufen.~n',[]),
    !.

    
% todo muss VOR der richtigen dependency definition stehen
% schliesst diese aus !!!! darf nicht gleigzeitig consultiert sein
posDepend(_ct1, _ct2, _label) :-
    use_cache_results_only,
    !,
    ct_edge(_ct1, _ct2, _label, positive).
negDepend(_ct1, _ct2, _label) :-
    use_cache_results_only,
    !,
    ct_edge(_ct1, _ct2, _label, negative).


gen_node(_stream, _node) :-
    term_to_atom(_node, _atom),
    format(_stream, 'ct_node(~a)~n', [_atom]),
    assert(ct_node(_atom)).

gen_edge(_stream, _node1, _node2, _label, _type) :-
    term_to_atom(_label, _atom),
    term_to_atom(_node1, _atom1),
    term_to_atom(_node2, _atom2),
    format(_stream, 'ct_edge(~a, ~a, ~a, ~a)~n', [_atom1, _atom2, _atom, _type]),
    assert(ct_edge(_node1, _node2, _atom, _type)).

gen_order(_stream, _ctlist) :-
    topo_sort(_ctlist, _order),
    !,
    maplist(term_to_atom, _order, _order2),
    concat_atom(_order2,', ',_orderstr),
    format(_stream, 'ct_order(~a)~n~n',[_orderstr]),
    assert(ct_order(_order)).
gen_order(_stream, _ctlist) :-
    %format(_stream, 'no valid order, solve the following problems:~n~n', []),
    
    setof(C1, circle(_ctlist, C1), L1),
    checklist( gen_circle(_stream), L1),
    
    format('~n',[]),
    setof(C2, conflict(_ctlist, C2), L2),
    checklist( gen_conflict(_stream), L2).


gen_circle(_stream, _l1) :-
    maplist(term_to_atom, _l1, _l),
    concat_atom(_l,', ', _a),
    format(_stream, 'ct_circle([~a])~n',[_a]),
    assert(ct_circle(_l)).

gen_conflict(_stream, _l1) :-
    maplist(term_to_atom, _l1, _l),
    concat_atom(_l,', ', _a),
    format(_stream, 'ct_conflict([~a])~n',[_a]),
    assert(ct_conflict(_l)).
    


