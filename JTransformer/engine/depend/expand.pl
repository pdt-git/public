% Author:
% Date: 02.10.2002
:- multifile c/0.

/*************************** Expanding Abstraktions ****************************/

tree(a) :- !.
tree(b) :- !.
tree(c) :- !.
tree(d) :- !.
tree(e) :- !.



/**
 * may generate multiple _potentialDependencies lists via backtracking
 * fails if list is empty
*/
expandAndCollect(_pre, _act, _Deps) :-
    expandActions(_act, _l1),
    list_to_set(_l1, _expandedAct),
    expandConditions(_pre, _l2),
    list_to_set(_l2, _expandedCond),
    findall(_elem, posNegMember(_expandedCond, _expandedAct, _elem), _Deps),
    _Deps \= [].
%test('expandAndCollect#1') :- findall(_deps,expandAndCollect([a,s],[a,r],_deps),[[a], [a, c], [a, b, d], [a, d]]).
test('expandAndCollect#2') :- findall(_deps,expandAndCollect([],[a,r],_deps),[]).
test('expandAndCollect#3') :- findall(_deps,expandAndCollect([a,s],[],_deps),[]).

expand(_pre, _act, _ExpandedCond, _ExpandedAct) :-
    expandActions(_act, _l1),
    list_to_set(_l1, _ExpandedAct),
    expandConditions(_pre, _l2),
    list_to_set(_l2, _ExpandedCond).


action(e) :-
    action(d).
action(r) :-
    action(a),
    action(q),
    action_all(c).
action(r) :-
    action(b),
    action(e),
    action(f).
action(f) :-
    action(d).

%test('expandActions#1') :- findall(_solution, expandActions([a,r],_solution), [[a, a, c], [a, b, e, d, d]]).
%test('expandActions#2') :- findall(_solution, expandActions([],_solution), [[]]).
%test('expandActions#1') :- findall(_solution, expandActions([a,r],_solution), [[a, r, a, q, c], [a, r, b, e, d, f, d]]).
%test('expandActions#2') :- findall(_solution, expandActions([],_solution), [[]]).

expandActions([],[]).
expandActions([_h|_t],_T) :-
    expandAction(_h, _t1),
    expandActions(_t, _t2),
    append(_t1, _t2, _T).


/*
expandAction(_abstraction, [_abstraction|_T]) :-
    tree(_abstraction),
    clause(action(_abstraction), _body),
    comma2list(_body, _bodylist),
    action_member(_abstraction, _bodylist, _T).
*/
expandAction(_abstraction, _T) :-
    not(tree(_abstraction)),
    clause(action(_abstraction), _body),
    comma2list(_body, _bodylist),
    action_member(_abstraction, _bodylist, _T).
expandAction(_abstraction, [_abstraction]) :-
    tree(_abstraction).
%    not(clause(action(_abstraction), _)).
expandAction(_abstraction, []) :-
    not(tree(_abstraction)),
    not(clause(action(_abstraction), _)).

%todo - konvention was in actions als retract und assert keywords benutzt werden dürfen
%       action(replace(classDefT(...))) sollte diese keywords NICHT enthalten, da für replace sonderbehandlung gemacht wird
action_member(_, [], []) :- !.
action_member(_abstraction, [retractT(_member)|_t], [not(_member)|_T]) :-
    tree(_member),
    %noRecursion(_abstraction, _member),
    !,
    action_member(_abstraction, _t, _T).
action_member(_abstraction, [assert1T(_member)|_t], [_member|_T]) :-
    tree(_member),
    %noRecursion(_abstraction, _member),
    !,
    action_member(_abstraction, _t, _T).
action_member(_abstraction, [action(_member)|_t], _T) :-
    %noRecursion(_abstraction, _member),
    !,
    expandAction(_member, _t1),
    action_member(_abstraction, _t, _t2),
    append(_t1, _t2, _T).
action_member(_abstraction, [action_all(_member)|_t], _T) :-
    %noRecursion(_abstraction, _member),
    !,
    expandAction(_member, _t1),
    action_member(_abstraction, _t, _t2),
    append(_t1, _t2, _T).
action_member(_abstraction, [_h|_t], _T) :-
    !,
    action_member(_abstraction, _t, _T).


/*
expandActions([],[]).
expandActions([_h|_t],[_h|_t3]) :-
    expandAction(_h, _t1),
    expandActions(_t, _t2),
    append(_t1, _t2, _t3).
expandActions([_h|_t],[_h|_t2]) :-
    not(expandAction(_h, _t1)),
    expandActions(_t, _t2).

expandAction(_abstraction, _T) :-
    clause(action(_abstraction), _body),
    comma2list(_body, _bodylist),
    action_member(_abstraction, _bodylist, _actionlist),
    expandActions(_actionlist, _T).
*/



/*****************************************/

cond(s).
s :-
    b,
    u.
s :-
    a,
    c.
cond(c).
c :-
    d.
cond(u).
u :-
    d.
%test('expandConditions#1') :- findall(_solution, expandConditions([a,s],_solution), [[a, b, d], [a, a, c, d]]).
%test('expandConditions#2') :- findall(_solution, expandConditions([],_solution), [[]]).
%test('expandConditions#1') :- findall(_solution, expandConditions([a,s],_solution), [[a, s, b, u, d], [a, s, a, c, d]]).
%test('expandConditions#2') :- findall(_solution, expandConditions([],_solution), [[]]).


expandConditions([],[]).
expandConditions([_h|_t],_T) :-
    expandCondition(_h, _t1),
    expandConditions(_t, _t2),
    append(_t1, _t2, _T).

/*
expandCondition(_abstraction, [_abstraction|_T]) :-
    tree(_abstraction),
    cond(_abstraction),
    clause(_abstraction, _body),
    comma2list(_body, _bodylist),
    expandConditions(_bodylist, _T).
*/
expandCondition(_abstraction, _T) :-
    cond(_abstraction),
    not(tree(_abstraction)),
    clause(_abstraction, _body),
    comma2list(_body, _bodylist),
    expandConditions(_bodylist, _T).
expandCondition(_abstraction, [_abstraction]) :-
    tree(_abstraction).
%    not(cond(_abstraction)).
expandCondition(_abstraction, []) :-
    not(tree(_abstraction)),
    not(cond(_abstraction)).

/*
cond_member(_, [], []) :- !.
cond_member(_abstraction, [_member|_t], [_member|_T]) :-
    tree(_member),
    noRecursion(_abstraction, _member),
    !,
    cond_member(_abstraction, _t, _T).
cond_member(_abstraction, [_member|_t], [_member|_T]) :-
% statt nur auf dependency zu prüfen, zu restriktiv, lower level werden nicht reference checked->
%           auf tree struktur prüfen
    functor(_member, _f, _a),
    treeFact(_f/_a),
    noRecursion(_abstraction, _member),
    !,
    cond_member(_abstraction, _t, _T).

cond_member(_abstraction, [_member|_t], _T) :-
    cond(_member),
    noRecursion(_abstraction, _member),
    !,
    expandCondition(_member, _expMember),
    append(_expMember, _T2, _T),
    cond_member(_abstraction, _t, _T2).
cond_member(_abstraction, [_h|_t], _T) :-
    !,
    cond_member(_abstraction, _t, _T).
*/


/*
    findall(_member, condMember(_abstraction, _member, _body), _l),
    expandConditions(_l, _T).
condMember(_abstraction, _member, _body) :-
    comma_member(_member, _body),
    tree(_member),
    noRecursion(_abstraction, _member).
condMember(_abstraction, _member, _body) :-
    comma_member(_member, _body),
    not(tree(_member)),
    cond(_member),
    noRecursion(_abstraction, _member).
*/

/*
addtree(_elem, _l, [_elem|_l]).
addtree(_elem, _l, [_elem|_l]) :-
    tree(_elem),
    !.
addtree(_elem, _l, _l).
*/





/*abstractionConditiontree(_abstraction, _Dependency) :-
    cond(_abstraction),
    clause(_abstraction, _body),
    comma_member(_Dependency, _body),
    tree(_Dependency).
abstractionConditiontree(_abstraction, _Dependency) :-
    cond(_abstraction),
    clause(_abstraction, _body),
    comma_member(_another, _body),
    cond(_another),
    noRecursion(_abstraction, _another),
    abstractionConditiontree(_another, _Dependency).
*/

