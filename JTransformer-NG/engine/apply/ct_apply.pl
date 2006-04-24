/*
Interpretor for the following language

Conditions:
    C = EC | C‘, ‘C | C‘; ‘C | ‘not((‘C‘))‘
    EC = tree | true | false
    
Actions:
    T = ET | ET‘, ‘T
    ET = add(tree) | delete(tree) | replace(tree1, tree2) | empty
    
CT's:
    ct(_name, _preCond, _action).
*/

/**
 * debug_apply_action_output
 * 
 * deactivated by default.
 */
:- dynamic debug_apply_action_output/0.
:- dynamic applied/1.

:- dynamic lastApplyInfo/1.
:- dynamic lastApplyListInfo/1.
:- dynamic rollback/1.
:- multifile rollback/1.

:- multifile action/1.
:- multifile ct/3.
:- dynamic ct/3.
:- multifile test/1.

/**
 * t_i(+Id,+Goal)
 *
 * t_i is short for term_info.
 * Id is ct-wide unique identifier for Goal.
 * If the Goal throws the exception 
 * term_info_exception(PredicateName,Args)}}
 * the predicate PredicateName(ID,Arg1,..) will be called.
 *
 * This enables expressive error 
 * handling for the predicates in Goal, since
 * the predicate (error handling) PredicateName is provided 
 * with the associated abba:node
 * for the currently evaluated predicate Goal.
 */

t_i(Id,Goal) :-
  catch(Goal, Exception,true),
%  debug_t_i(Exception),
  term_info_exception_handling(Id, Exception).

%  debug_t_i(Exception):-
%      var(Exception),
%      !.
%  
%  debug_t_i(Exception):-
%      debugme.

term_info_exception_handling(_, Exception):-
  var(Exception),
  !.

/**
 * term_info_exception_handling(+Id, +term_info_exception(Predicate,[Arg1,..]))
 *
 * Checks if the predicate Predicate(Id,Arg1,..)
 * exists and calls this predicate if this is the case.
 */
term_info_exception_handling(Id, term_info_exception(Predicate,Args)):-
  Call =.. [Predicate, Id |Args],
  current_predicate(Predicate,_), % check existance
  !,
  call(Call).

term_info_exception_handling( _Id, Exception):-
   throw(Exception).


/** apply all ct's **/
apply_ctlist([]):-!.
apply_ctlist(List) :-
	apply_ctlist_(List,ApplyInfo),
	retractall(lastApplyListInfo(_)),
	assert(lastApplyListInfo(ApplyInfo)),
	length(ApplyInfo,Num),
	format('~n===========================~n~napplied ~a CT:~n',[Num]),
	forall(member(M,ApplyInfo),format('~w~n',M)).

showApplyListInfo :-
    lastApplyListInfo(ApplyInfo),
	length(ApplyInfo,Num),
	format('~n===========================~n~napplied ~a CT:~n',[Num]),
    forall(member(M,ApplyInfo),format('~w~n',M)).
    
	
apply_ctlist_([],[]).
apply_ctlist_([_head|_tail],[S|ApplyInfoRest]) :-
        apply_ct(_head),
        lastApplyInfo(S),
        !,
        apply_ctlist_(_tail,ApplyInfoRest).


check(_action) :- call(_action).
apply(empty).

apply(_action) :- call(_action).
%    write('action1:\n'),write(_action).

% check and apply work independend
apply_ct(_name) :-
    ct(_name, _preConditionDA, _action),
    retractall(pointcut(_)),
    removeDependencyInstructions(_preConditionDA, _preCondition),
    findall( [_name, _action],apply_pre(_preCondition,_action),_allActions),
    %quicksort(_allActions,_uniqueActions,[]),
    apply_all_post(_allActions),
    add_apply_info_(_name,_allActions),
    assert1T(applied(_name)).


apply_all_post([]):-!.
apply_all_post([[Info,_h]|_t]) :-
    debug_apply_all_post(Info),
   % format('~n~w',[_h]),
    comma2list(_h,_l),
    apply_post(_l),
    retractall(pointcut(_)),
    apply_all_post(_t),
    !.
apply_all_post(_).

debug_apply_all_post(Info):-
    debug_apply_action_output,
    format('action:~w~n',[Info]).
debug_apply_all_post(_Info).
    
toggle_apply_action_debug :-
    debug_apply_action_output,
    !,
    format('no apply action debug output ~n',[]),
    retractall(debug_apply_action_output).
    
toggle_apply_action_debug  :-
    format('apply action debug output ~n',[]),
        assert(debug_apply_action_output).
        
    
apply_post([]).
apply_post([_head| _tail]) :-
    clause(action(_head),_),
    !,
    error_handling(action(_head),'apply_post action failed: ~a',[_head]),
    !,
    apply_post(_tail).
    
apply_post([t_i(Id,Action)| _tail]) :-
    clause(action(Action),_),
    !,
    error_handling(t_i(Id,action(Action)),'apply_post action failed: ~a',[Action]),
    !,
    apply_post(_tail).

apply_post([_head| _tail]) :-
    error_handling(fail,'apply_post action failed: could not find action ~a~n',[_head]),
    !.

/*
apply_post([not(_head)| _tail]) :-
        !,
%        not(clause(action(not(_head)),_)),
        retractT(_head),
        !,
        apply_post(_tail).
apply_post([_head| _tail]) :-
%        _head \= not(_),
%        not(clause(action(_head),_)),
        assert1T(_head),
        !,
        apply_post(_tail).
*/

/**
 *  add_apply_info_(+_name,+_allActions)
 */
add_apply_info_(_name,_allActions):-
    length(_allActions,Num),
	sformat(S,'applied ~w actions for the ct "~w"',[Num,_name]),
	format('~w~n================================================~n', [S]),
	retractall(lastApplyInfo(_)),
	assert(lastApplyInfo(S)).
	
 

/** check if all preconditions can be applied on fact base **/
apply_pre(_pre,_action) :-
% if call succeeds, unification
        call(_pre).

% effekts of apply may cause check to backtrack -> may express recursion
applyCTbacktrack(_name/*, (_preCondition)*/) :-
    ct(_name, (_preCondition), (_action)),
    check((_preCondition)),
    apply((_action)).


/*
 * dnf/2, simplify/2 and helper predicates moved to Condor
 * (file depend/dnf.pl) since they are only used there.
 */


% Entferne Anweisungen an die Analyse
removeDependencyInstructions(_member, (_memberExp)) :-
    _member \= ','(_,_),
    _member \= ';'(_,_),
    removeDependencyInstruction(_member, _memberExp).
removeDependencyInstructions(','(_member,_t), ','(_memberExp,_T)) :-
    removeDependencyInstruction(_member, _memberExp),
    removeDependencyInstructions(_t, _T).
removeDependencyInstructions(';'(_member,_t), (';'(_memberExp,_T))) :-
    removeDependencyInstruction(_member, _memberExp),
    removeDependencyInstructions(_t, _T).

removeDependencyInstruction(dependency_analysis(_command),true):- !.
removeDependencyInstruction(_command,_command):- !.


/**
 *	add(+pef)
 *	Adds +pef to the factbase.
 *	pef must be a bound PEF term. The addition to the fact base is logged and
 *	will be undone in the next rollback.
 *	
 *	WARNING (Not finally agreed on): If the pef already 
 *	exists the predicate is ignored. The pef is not 
 *	added to the factbase, no rollback information is added.
 */

add(java_fq(Elem)):-
    !,
    java_fq_to_pef(Elem,PEF),
    add(PEF).

add(Elem) :-
    nonvar(Elem), 
	call(Elem),
	!,
	format('~nWARNING: element: already exists: ~w~n.',[Elem]).

/*
add(Elem) :-
    nonvar(Elem), 
    not(tree(Elem)),
	!,
	error_handling(fail, 'ERROR: element is not a tree: ~w~n.',[Elem]).
*/	

add(_elem) :-  
    nonvar(_elem), 
    assert(_elem),
    asserta(rollback(retract(_elem))),
    markEnclAsDirty(_elem).

    
/*
	delete(+pef)
	Deletes +pef from the factbase.
	pef must be a bound PEF term. The deletion from the fact base is logged and
	will be undone in the next rollback.
*/

delete(java_fq(Elem)):-
    !,
    java_fq_to_pef(Elem,PEF),
    delete(PEF).
    
delete(_elem) :- 
        nonvar(_elem), 
        retract(_elem), 
        markEnclAsDirty(_elem),
        asserta(rollback(assert(_elem))).

replace(java_fq(Elem1),java_fq(Elem2)):-
    !,
    java_fq_to_pef(Elem1,PEF1),
    java_fq_to_pef(Elem2,PEF2),
    replace(PEF1,PEF2).

replace(_elem1, _elem2) :- delete(_elem1), add(_elem2).

replace(Elem) :- 
    Elem =.. [_|[ID|_]],
    getTerm(ID,ElemOld),
%    term_to_atom(ElemOld,A),
%    print(A),
    flush_output,
	delete(ElemOld), 
	add(Elem).

	