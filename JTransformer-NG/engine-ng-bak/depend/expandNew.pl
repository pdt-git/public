% Author:
% Date: 02.10.2002
:- multifile c/0.

/*************************** Expanding Abstractions **************************
 * Public: 
 *  expandAndCollect(+_precond, +_action, ?_Deps) ???
 *  expand(+_preconditions, +_actions, ?_ExpandedConds, ?_ExpandedActs) ???
 ****************************************************************************/


/**
 * expandAndCollect(+_precond, +_action, ?_Deps)
 *
 * May generate multiple _potentialDependency lists (arg3) via backtracking.
 * Fails if result list in arg3 is empty.
 *
 * ??? What should this do? I don't see how this can generate dependency lists.
 * !!! TODO: The predicate posNegMember/3 called by findall/3 is undefined :(
*/
expandAndCollect(_pre, _act, _Deps) :-
    expandActions(_act, _actList),
    list_to_set(_actList, _actSet),   % _actSet retains only first occurrence of elements from _actList
    expandConditions(_pre, _preList),
    list_to_set(_preList, _preSet),  % _preSet retains only first occurrence of elements from _preList
    findall(_elem, posNegMember(_expandedCond, _expandedAct, _elem), _Deps),
    _Deps \= [].

/** 
 * expand(+_preconditions, +_actions, ?_ExpandedConds, ?_ExpandedActs)
 *
 * arg3 (_ExpandedConds) is the list obtained by expanding the literals from
 * arg1 (_preconditions) via  expandConditions/2
 * The expansion uses the cond/1 predicate from the file "high_level_api.pl"
 *
 * arg4 (_ExpandedActs) is the list obtained by expanding the literals from
 * arg2 (_actions) via expandActions/2
 * The expansion uses the action/1 predicate from the file "high_level_api.pl"
 *
 */
expand(_pres, _acts, _ExpandedConds, _ExpandedActs) :-
    expandActions(_acts, _l1),
    list_to_set(_l1, _ExpandedActs),
    expandConditions(_pres, _l2),
    list_to_set(_l2, _ExpandedConds).


/* ================= ACTIONS ============================================= */

/** expandActions(+actList,?ExpandedActs)
 *
 * arg2 (_ExpandedActs) is the list obtained by expanding each literal from
 * arg1 (_actions) via expandAction/2. The expansion proceeds depth first.
 * The relative order of literals is preserved.
 */
expandActions([],[]).
expandActions([_h|_t],_T) :-
    expandAction(_h, _t1),
    expandActions(_t, _t2),
    append(_t1, _t2, _T).

/** 
 * expandAction(+Literal, ?ExpansionList)
 *
 * arg2 (ExpansionLst) is obtained by expanding arg1 (Literal) as follows:
 *   - If tree(Literal) is true, Literal is included into ExpansionList.
 *   - If tree(Literal) is false and there is no definition of an
 *     action(Literal) clause where Literal unifies with arg1 the 
 *     literal is ignored (the ExpansionList is empty).
 *   - Otherwise, the body of the matching action clause(s) is scanned for 
 *     relevant literals using expandActionBody/2. If multiple clauses 
 *     match, the predicate returns multiple results (possibly empty). 
 *
 * NOTE: The predicate could be twice as fast if the comma2list/2 call
 * in the third clause would be eliminated and the expandActionBody/2
 * predicate would use AND-terms instead of lists in the first argument. 
 * However, the current version is more readable since it does not
 * mix two concerns (expansion and conversin of AND terms to lists).
 */
expandAction(_pef, [_pef]) :-                     % preserve PEF
    tree(_pef),
    !.
expandAction(_abstraction, []) :-                 % ignore non-expandable
    not(clause(action(_abstraction), _)),
    !.
expandAction(_abstraction, _T) :-                 % expansion
    clause(action(_abstraction), _body),
    comma2list(_body, _bodylist),
    expandActionBody(_bodylist, _T).
/*
   Version of previous clause that includes the expanded literal in 
   the result list in front of its expansion: 
expandAction(_abstraction, [_abstraction|_T]) :-
    clause(action(_abstraction), _body),
    comma2list(_body, _bodylist),
    action_member(_bodylist, _T).
*/


/** 
 * expandActionBody(+ActionList, ?ExpansionList)
 * 
 * For each element L of arg1, arg2 contains either its expansion E
 * obtained via expandActionLiteral(L,E) or nothing. That is, 
 * literals that do not qualify as expandable actions are ignored.
 */
expandActionBody([], []) :-                        % empty list
	!.
expandActionBody([_h|_t], _htExpanded) :-          % expansion    
    expandActionLiteral(_h,_hExpanded),
    !,
    expandActionBody(_t, _tExpanded),
    append(_hExpanded, _tExpanded, _htExpanded).
expandActionBody([_h|_t], _T) :-                   % ignore non-expandable
    !,
    expandActionBody(_t, _T).


/** 
 * expandActionLiteral(+Literal, ?Expansion)
 * 
 * arg2 is the expansion (Literal or List) of arg1.
 * 
 */
%TODO - Konvention was in actions als retract und assert keywords benutzt werden dürfen.
%       Die action(replace(classDefT(...))) sollte diese keywords NICHT enthalten, da 
%       für replace sonderbehandlung gemacht wird.
expandActionLiteral(retractT(_pef),not(_pef)) :-
	tree(_pef),
	!.
expandActionLiteral(assert1T(_pef),_pef) :-
	tree(_pef),
	!.
expandActionLiteral(action(_act),_actExpanded) :-
    !,
    expandAction(_act, _actExpanded).
expandActionLiteral(action_all(_act),_actExpanded) :-
    expandAction(_act, _actExpanded).


/* ================= CONDITIONS ============================================= */

expandConditions([],[]).
expandConditions([_h|_t],_T) :-
    expandCondition(_h, _t1),
    expandConditions(_t, _t2),
    append(_t1, _t2, _T).


expandCondition(_pef, [_pef]) :-         % preserve PEFs
    tree(_pef),
    !.
expandCondition(_abstraction, []) :-     % ignore non-expandable
    not(cond(_abstraction)),
    !.
expandCondition(_abstraction, _T) :-     % expand body
    clause(_abstraction, _body),
    comma2list(_body, _bodylist),
    expandConditions(_bodylist, _T).
/*
   Version of previous clause that includes the expanded literal in 
   the result list in front of its expansion: 
expandCondition(_abstraction, [_abstraction|_T]) :-
    clause(_abstraction, _body),
    comma2list(_body, _bodylist),
    expandConditions(_bodylist, _T).
*/
