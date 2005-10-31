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
 * i_t(+Id,+Goal)
 *
 * i_t is short for interpret_term.
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

i_t(Id,Goal) :-
  catch(Goal, Exception,true),
  term_info_exception_handling(Id, Exception).

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
  current_predicate(_,Call), % check existance
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
    
apply_post([i_t(_Id,Action)| _tail]) :-
    clause(action(Action),_),
    !,
    error_handling(action(Action),'apply_post action failed: ~a',[Action]),
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
DNF1 : dnf(True) -> True
DNF2 : dnf(False) -> False
DNF3 : dnf(Atom(x)) -> Atom(x)
DNF4 : dnf(Not(x)) -> not(dnf(x))
DNF5 : dnf(And(x,y)) -> and(dnf(x),dnf(y))
DNF6 : dnf(Or(x,y)) -> Or(dnf(x),dnf(y))

AND1 : and(Or(x,y),z) -> Or(and(x,z),and(y,z))
AND2 : and(z,Or(x,y)) -> Or(and(z,x),and(z,y))
AND3 : and(x,y) -> And(x,y) (default)

NOT1 : not(Not(x)) -> x
NOT2 : not(And(x,y)) -> Or(not(x),not(y))
NOT3 : not(Or(x,y)) -> and(not(x),not(y))
NOT4 : not(x) -> Not(x) (default)
*/
/*
dnf(true, true).
dnf(false,false).
dnf(_term,_term):-
    atomic(_term).

dnf(not(_term), _ReTerm):-
    dnf(_term,_tmp),
    notdnf(not(_tmp), _Term),
    dnf(_Term,_ReTerm),
    !.
dnf(not(_term), not(_term)).

dnf(and(_term1,_term2), _ReTerm) :-
    dnf(_term1,_tmp1),
    dnf(_term2,_tmp2),
    anddnf(and(_tmp1,_tmp2),_Term),
    dnf(_Term, _ReTerm),
    !.
dnf(and(_term1,_term2), and(_term1,_term2)).

    
dnf(or(_term1,_term2), or(_Term1,_Term2)):-
    dnf(_term1,_Term1),
    dnf(_term2,_Term2).

anddnf(and(or(_x,_y),_z),or(and(_x,_z),and(_y,_z))):-!.
anddnf(and(_z,or(_x,_y)),or(and(_z,_x),and(_z,_y))):-!.
%anddnf(and(_x,_y),and(_x,_y)).

notdnf(not(not(_x)),_x):-!.
notdnf(not(and(_x,_y)),or(not(_x),not(_y))):-!.
notdnf(not(or(_x,_y)),and(not(_x),not(_y))):-!.
%notdnf(not(_x),not(_x)).
*/


%dnf_vars(_term, _Term) :-
%    dnf(_term, _atom_term),
%    unwrapVars(_atom_term, _Term).


%dnf(_term,wrappedVar(_term)):-
%    var(_term),
%    !.

%    term_to_atom(_term, _Atom),
%    assert1T(varTable(_Atom,[_Atom = _term])).

%dnf(wrappedVar(_term),wrappedVar(_term)).

dnf(true, true)     :- !.
dnf(false,false)    :- !.

dnf(not(_term), _Term):-
    dnf(_term,_tmp),
    notdnf(not(_tmp), _Term),
    !.

dnf(','(_term1,_term2), _Term) :-
    dnf(_term1,_tmp1),
    dnf(_term2,_tmp2),
    anddnf(','(_tmp1,_tmp2),_Term2),
    simplify(_Term2, _Term),
    !.

dnf(';'(_term1,_term2), _Term):-
    dnf(_term1,_Term1),
    dnf(_term2,_Term2),
    ordnf(';'(_Term1,_Term2), _Term),
    !.

dnf(_term,_term). % normales literal: weder true, false, not/1, ','/2, ';'/2


anddnf(','(';'(_x,_y),_z),_X):-  !, dnf(';'(','(_x,_z),','(_y,_z)),_X).
anddnf(','(_z,';'(_x,_y)),_X):-  !, dnf(';'(','(_z,_x),','(_z,_y)),_X).
anddnf(','(','(_a,_b),_c),_X):- !, dnf(','(_a,','(_b,_c)),_X).
anddnf(','(_x,_y),','(_x,_y)).


ordnf(';'(';'(_a,_b),_c),_X) :- !, dnf(';'(_a,';'(_b,_c)), _X).
ordnf(';'(_term1,_term2), ';'(_term1,_term2)).


notdnf(not(not(_x)),_X)     :- !, dnf(_x,_X).
notdnf(not(','(_x,_y)),_X)  :- !, dnf(';'(not(_x),not(_y)),_X).
notdnf(not(';'(_x,_y)),_X)  :- !, dnf(','(not(_x),not(_y)),_X).
notdnf(not(_x),not(_x)).



simplify(_h, _h) :-
    _h \= ','(_,_),
    !.
simplify(','(_h, _t), _T) :- % streicht später vorkommende literale (alternativ durch true ersetzen)
    comma_member(_m, _t),
    _m == _h,
    !,
    simplify(_t, _T).
simplify(','(_h, _t), ','(false,_T)) :- % ersetzt später negiert vorkommende literale durch false
    comma_member(_m, _t),
    (_m == not(_h); not(_m) == _h),
    !,
    simplify(_t, _T).
simplify(','(_h, _t), ','(_h,_T)) :-
    simplify(_t, _T).


/*
unwrapVars(_term, _Translated) :-
    _term =.. [_name | _args],
    echangeVarNamesWithVar(_args, _translatedArgs),
    _Translated =.. [_name | _translatedArgs].

echangeVarNamesWithVar([], []) :- !.
echangeVarNamesWithVar([wrappedVar(_var) | _t], [_var | _rest]) :-
%    atom_to_term(_varname,_Term, _binding),
    echangeVarNamesWithVar(_t, _rest).

echangeVarNamesWithVar([_term | _t], [_Term | _rest]) :-
    not(atomic(_term)),
    !,
    unwrapVars(_term,_Term),
    echangeVarNamesWithVar(_t, _rest).
echangeVarNamesWithVar([_term | _t], [_term | _rest]) :-
    echangeVarNamesWithVar(_t, _rest).
*/

test('dnf_and')     :-  dnf(((1,2),3,4),(1,2,3,4)).
test('dnf_or')      :-   dnf(((1;2);3;4),(1;2;3;4)).
test('dnf_complex1'):- dnf((1,(2,not((4,(7;6)));3)),(1, 2, not(4);1, 2, not(4), not(6);1, 2, not(7), not(4);1, 2, not(7), not(6);1, 3)).

test('dnf_term')    :- dnf( (var(V,C,x),(class(C,P,ooo);package(P,foo))), (var(V, C, x), class(C, P,ooo);var(V, C, x), package(P, foo))).

test(dnf_simple1) :- dnf((a,b,a),(b,a)).
test(dnf_simple2) :- dnf((not(a),b,not(a)),(b,not(a))).
test(dnf_simple3) :- dnf((not(a),b,a),(false,b,a)).
test(dnf_simple4) :- dnf((a,b,not(a)),(false,b,not(a))).
test(dnf_simple5) :- dnf((a(X),b,a(X)),(b,a(X))).
test(dnf_simple6) :- dnf((a(Y),b,a(X)),(a(Y),b,a(X))).

%test('dnf_or') :-
%    dnf(((1;2);3;4),(1;2;3;4)).
%test('dnf_vars') :-
%    dnf_vars((4,(_a;((a;_b;not(_a)),b))),(4, _a;4, a, b;4, _b, b;4, not(_a), b)).


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

	