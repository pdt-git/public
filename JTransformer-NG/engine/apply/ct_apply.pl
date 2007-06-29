/**
 * Interpretor for the following language
 * 
 * Conditions:
 *     C = EC | C‘, ‘C | C‘; ‘C | ‘not((‘C‘))‘
 *     EC = tree | true | false
 *     
 * Actions:
 *     T = ET | ET‘, ‘T
 *     ET = add(tree) | delete(tree) | replace(tree1, tree2) | empty
 *     
 * CT's:
 *     ct(_name, _preCond, _action).
 *     
 * Debugging for this module can be deactivated with nodebug(applyct).
 *   
 */

/**
 * debug_apply_action_output
 * 
 * deactivated by default.
 */
:- dynamic debug_apply_action_output/0.

% activate: debugging for this module:
:- debug(applyct).

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
 * advice_cache(?VariableBindings, ?AdviceClass)
 *
 * Relation between the ct action variable bindings
 * and cached advice classes.
 */
:- dynamic advice_cache/2.

/**
 * advice_generated_from_ct(?CTNameTerm,?AdviceMethodId)
 * 
 * Holds advice classes generated in
 * the last application of the ct CTNameTerm.
 * 
 */
:- dynamic advice_generated_from_ct/2.

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
	debug(applyct,'~n===========================~n~napplied ~w CT:~n',[Num]),
	forall(member(M,ApplyInfo),debug(applyct,'~w',M)).

showApplyListInfo :-
    lastApplyListInfo(ApplyInfo),
	length(ApplyInfo,Num),
	debug(applyct,'~n===========================~n~napplied ~w CT:~n',[Num]),
    forall(member(M,ApplyInfo),debug(applyct,'~w',M)).
    
	
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
apply_ct(Name) :-
    ct(Name, PreConditionDA, Action),
    retractall(pointcut(_)),
    retractall(advice_generated_from_ct(Name,_)),
    removeDependencyInstructions(PreConditionDA, PreCondition),
    term_variables(Action,Variables),
    findall( [Name, Variables, Action],call(PreCondition),AllActions),
    %quicksort(_allActions,_uniqueActions,[]),
    apply_all_post(AllActions),
    add_apply_info_(Name,AllActions),
    assert1T(applied(Name)).


apply_all_post([]):-!.
apply_all_post([[Name,ActionVariables,Head]|Tail]) :-
    debug_apply_all_post(Name),
    comma2list(Head,List),
    ( 
      advice_cache(ActionVariables,Cached)
        ->
         assert(current_advice(Cached))
       ;
       (
         apply_post(List),
         update_laj_advice_cache(Name,ActionVariables)
       ) 
    ),
    retractall(pointcut(_)),
    apply_all_post(Tail),
    !.
apply_all_post(_).

/**
 * update_laj_advice_cache(+CTNameTerm,-ActionVariables)
 *
 * Extracts the AdviceMethod id of a ctname term.
 * If the ct is not an advice ct the predicate fails.
 */

update_laj_advice_cache(CTNameTerm,ActionVariables):-
   laj_advice_method(CTNameTerm,AdviceMethod),
   !,
   assert(advice_cache(ActionVariables,AdviceMethod)),
   assert(advice_generated_from_ct(CTNameTerm,AdviceMethod)).
   
update_laj_advice_cache(__Name,__ActionVariables).

/**
 * laj_advice_method(+CTNameTerm,-AdviceMethod)
 *
 * Extracts the AdviceMethod id of a ctname term.
 * If the ct is not an advice ct the predicate fails.
 */
 
laj_advice_method(CTNameTerm,AdviceMethod) :-
    CTNameTerm =.. [__CTname,__jp,advicemethod(AdviceMethod)].

debug_apply_all_post(Info):-
    debug_apply_action_output,
    debug(applyct,'action:~w~n',[Info]).
debug_apply_all_post(_Info).
    
toggle_apply_action_debug :-
    debug_apply_action_output,
    !,
    debug(applyct,'no apply action debug output ~n',[]),
    retractall(debug_apply_action_output).
    
toggle_apply_action_debug  :-
    debug(applyct,'apply action debug output ~n',[]),
        assert(debug_apply_action_output).
        
    
apply_post([]).
apply_post([_head| _tail]) :-
    clause(action(_head),_),
    !,
    error_handling(action(_head),'apply_post action failed: ~w',[_head]),
    !,
    apply_post(_tail).
    
apply_post([t_i(Id,Action)| _tail]) :-
    clause(action(Action),_),
    !,
    error_handling(t_i(Id,action(Action)),'apply_post action failed: ~w',[Action]),
    !,
    apply_post(_tail).

apply_post([_head| _tail]) :-
    error_handling(fail,'apply_post action failed: could not find action ~w~n',[_head]),
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
add_apply_info_(Name,_allActions):-
    length(_allActions,Num),
    debug(applyct, 'APPLY_CT: applied ~w actions for the ct "~w"~n',[Num,Name]),
	sformat(String,'applied ~w actions for the ct "~w"',[Num,Name]),
%	format('~w~n================================================~n', [S]),
	retractall(lastApplyInfo(_)),
	assert(lastApplyInfo(String)).
	
 

/** check if all preconditions can be applied on fact base **/
apply_pre(Pre) :-
% if call succeeds, unification
        call(Pre).

% effekts of apply may cause check to backtrack -> may express recursion
applyCTbacktrack(Name/*, (_preCondition)*/) :-
    ct(Name, (PreCondition), (Action)),
    check((PreCondition)),
    apply((Action)).


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
	debug(applyct,'~nWARNING: element: already exists: ~w~n.',[Elem]).

/*
add(Elem) :-
    nonvar(Elem), 
    not(tree(Elem)),
	!,
	error_handling(fail, 'ERROR: element is not a tree: ~w~n.',[Elem]).
*/	

add(Element) :-  
    nonvar(Element), 
    assert(Element),
    asserta(rollback(retract(Element))),
    addTreeReverseIndexes(Element),
    (  deterministic(false)
    -> debugme(1,2,3)
    ;
    true),
    markEnclAsDirty(Element).
    
    
/*
	assert1T(+Term)
	Asserts term Term. If Term already
	exists it will not be asserted.
	The predicate is always true.
*/
add1(Element) :- 
	call(Element),
	!.
add1(Element) :- 
	add(Element).
    
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
    
delete(Element) :- 
        nonvar(Element), 
        retract(Element), 
        markEnclAsDirty(Element),
        deleteTreeReverseIndexes(Element),
        asserta(rollback(assert(Element)))
        .
        

/*
	replace(+Pef1,+Pef2)

	replaces Pef1 with Pef2.
	Pef1 and Pef2 must be a bound PEF terms. 
	The replacement is logged and
	will be undone in the next rollback.
*/
replace(java_fq(Elem1),java_fq(Elem2)):-
    !,
    java_fq_to_pef(Elem1,PEF1),
    java_fq_to_pef(Elem2,PEF2),
    replace(PEF1,PEF2).

replace(Element1, Element2) :- delete(Element1), add(Element2).

replace(Elem) :- 
    Elem =.. [_|[ID|_]],
    getTerm(ID,ElemOld),
%    term_to_atom(ElemOld,A),
%    print(A),
    flush_output,
	delete(ElemOld), 
	add(Elem).

	