






/* Syntaktische Konvention:
    Terme die mit Grossbuchstaben anfangen, zeigen an, dass man dort
    Instantiierungen erwartet ("input") beim Aufruf des Prädikats.
 **********************************************************************
   In this file:
   - dependency computationj

   - preprocessing / normalisation of conditional transformer clauses
   - conditional transformer test data
   - topological sorting
 **********************************************************************
*/
/** Dependency */
/**
Eine Abhängigkeit zwischen zwei Transformationen besteht dann,
wenn in der Vorbedingung der einen und der nachbedingung der
anderen unifizierbare Literale existieren UND nach der unifikation
keine ungleichheitsbedingungen auswertbar werden und zu falsch
evaluieren.
**/
depends(_ct1,_ct2) :-
   depends(_ct1,_ct2,_condition).

depends(_ct1,_ct2,_condition) :-
        ct_filter(_ct1,_pre1,_post1,_preIneq1,_postIneq1),
        ct_filter(_ct2,_pre2,_post2,_preIneq2,_postIneq2),
        _ct1 \= _ct2,
        member(_condition,_pre1),
        member(_condition,_post2),
        checkInequalities(_preIneq1),
        checkInequalities(_postIneq2).
        

/* Transitive abhängigkeiten werden von der topologischen Sortierung
   aufgebaut. Ihre Erzeugung hier wäre daher redundant.
depends(_x,Y) :- depends(_x,T), depends(T,Y).
*/

/** Filtering: Get preprocessed conditional transformer clauses.
 **/
ct_filter(_name, _pre, _post, _preIneq, _postIneq) :-
       ct(_name, _pre1, _post1),
       removeNotAndEquals(_pre1 ,_pre ,_preIneq),
       removeNotAndEquals(_post1,_post,_postIneq).

/** Preprocessing of negation and equality literals:
    1. "equals(_t1,_t2)" literals from the first argument
       are pre-evaluated by unifying _t1 and _t2.
    2. "not(equals(_t1,_t2))" literals from the first argument
       can only be evaluated latter, during the computation of
       dependencies. They are extracted as "notequal(_var,_term)"
       literals into the third argument, to ease deferred processing.
    3. For other "not(_term)" literals from the first argument
       only _term is added to the second argument. These negations
       are irrelevant for the computation of dependencies.
 **/
   % _Lwith    = ListWithNegationAndEquality
   % _lwithout = listWithoutNegationAndEquality
   % _norm     = normalizednotequalList

removeNotAndEquals(_Lwith,_lwithout,_normalizednotequalList) :-
    removeNotAndEquals(_Lwith,_lwithout, [],_notequalList),
    normalizeInequality( _notequalList,_normalizednotequalList).

removeNotAndEquals([], [], _Ne, _Ne) :-
    !.
removeNotAndEquals([equals(_T1,_T2)|_Body], _body, _Ne, _ne) :-
    !,
    _T1 = _T2,
    removeNotAndEquals(_Body, _body, _Ne , _ne ).
removeNotAndEquals([not(equals(_T1,_T2))|_Body], _body, _Ne, [notequal(_T1,_T2)|_ne]) :-
    !,
    removeNotAndEquals(_Body, _body, _Ne , _ne ).
removeNotAndEquals([not(_Pred)|_Body], [_Pred|_body], _Ne, _ne) :-
    !,
    % not(_Pred=equals(_,_)) gilt bereits durch ! in vorheriger Klausel
    removeNotAndEquals(_Body, _body, _Ne , _ne ).
removeNotAndEquals([_Head|_Body], [_Head|_body], _Ne, _ne) :-
    %not(_Head = not(_x)) gilt bereits durch ! in vorheriger Klausel
    removeNotAndEquals(_Body, _body, _Ne, _ne ).

/** Normalization */
/** The list of inequalities in the first arguments is equivalent to
    the list of inequalities in the second argument. The latter
    contains only terms of the form "notequal(_variable,_term)" or
    or "disjunction(_reslist)" where _reslist is a list of such terms.
    The latter case represents a disjunction of inequalities.
 **/

normalizeInequality([],[]).
normalizeInequality([notequal(_T1,_T2)|_Body],[_head|_body]):-
    normalizeInequalityPair(_T1,_T2,_head),
    normalizeInequality(_Body,_body).

normalizeInequalityPair(_T1,_T2,notequal(_var,_term)) :-
   var(_T1),
   !,
   _var = _T1,
   _term = _T2.
normalizeInequalityPair(_T1,_T2,notequal(_var,_term)) :-
   var(_T2),
   !,
   _var = _T2,
   _term = _T1.
normalizeInequalityPair(_T1,_T2,disjunction(_reslist)) :-
   _T1 =.. [_functor1 | _args1],
   _T2 =.. [_functor2 | _args2],
   _functor1 = _functor2,
   normalizeInequalityLists(_args1,_args2,_reslist).
   % Die Bedingung "length(_args1, _n), length(_args2, _n)"
   % ist implzit in dem rekursiven Aufruf.

/**Die impliziten notequal-paare der argumentlisten normalisieren:
 **/
normalizeInequalityLists([],[],[]).
normalizeInequalityLists([_H1|_Body1],[_H2|_Body2],[_head|_body]) :-
   normalizeInequalityPair(_H1,_H2,_head),
   normalizeInequalityLists(_Body1,_Body2,_body).


/** Überprüfen, ob Ungleichungen nun auswertbar sind und gegebenenefalls
    auswerten. Eine normalisierte Ungleichung ist auswertbar, wenn ihre
    Variable instantiiert ist.
 **/
checkInequalities([]).
checkInequalities([disjunction(_List)|_Rest]) :- % var
   !,
   checkInequalities(_List),
   checkInequalities(_Rest).
checkInequalities([notequal(_Var,_Term)|_Rest]) :- % var
   var(_Var),
   !,
   checkInequalities(_Rest).
checkInequalities([notequal(_Var,_Term)|_Rest]) :- % nonvar
   _Var \= _Term,                              % eigentlicher check
   checkInequalities(_Rest).




/**********************************************************************/

/** Topo Sorting */

precedes(_x,_y) :- depends(_y,_x).
:- consult('topo_sort.pl').


/*
 *
 *       Applying of CT-List
 *
 *
 */


/** different kind of Filter's - alternative to above ct_filter **/

ct_depend_filter(_name, _pre, _post, _preIneq, _postIneq) :-
        ct(_name, _pre1, _post1),
        normalizeNE(_pre1, _pre2, _preIneq),
        normalizeNE(_post1,_post2, _postIneq),
        removeNot(  _pre2, _pre),
        removeNot(  _post2,_post).

ct_condition_filter(_name, _pre, _post, _preIneq, _postIneq) :-
        ct_action_filter(_name, _pre, _post, _preIneq, _postIneq).

ct_action_filter(_name, _pre, _post, _preIneq, _postIneq) :-
        ct(_name, _pre1, _post1),
        normalizeNE(_pre1, _pre, _preIneq),
        normalizeNE(_post1,_post, _postIneq).

        
/** some list manipulation **/  

normalizeNE(_Lwith, _lwithout, _normalizednotequalList) :-
        removeNE(_Lwith, _lreplaced),
        copy(  notequal(_x,_y),_lreplaced, _ne1),
        delete(notequal(_x,_y),_lreplaced, _lwithout),
        normalizeInequality( _ne1,_normalizednotequalList).
        
removeNE(_Lwith, _lwithout) :-
        subst(not(equals(_x,_y)),_Lwith,notequal(_x,_y),_lwithout). % not(not(...)) not suppoerted !

removeNot(_Lwith, _lwithout) :-
        subst(not(_x),_Lwith,_x,_lwithout). % not(not(...)) not suppoerted !

/** general List Tools from Clocksin/Mellish **/

/*
% delete(_x,L1,L2): construct new list L2 by deleting all occurences of _x in L1
delete(_,[],[]).
delete(_x,[_x|_l],_M) :- !, delete(_x,_l,_M).
delete(_x,[_y|_L1],[_y|_L2]) :- delete(_x,_L1,_L2).
*/


%  from Uwe: derived from delete
% copy(_x,L1,L2)  : construct new list L2 by copying only the occurences of _x in L1 and ignoring the others
copy(_x,[],[]).
copy(_x,[_x|_L1],[_x|_L2]) :- !, copy(_x,_L1,_L2).
copy(_x,[_y|_l],_M) :- copy(_x,_l,_M).

% from Uwe: 
% list2set(_l,S) : construct new set S by deleting all redundant elements out of list _l
list2set([],[]).
list2set(_l,_S) :- !, setof(_x,member(_x,_l),_S).

% subst(_x,_l,A,M) : construct new list M by replacing all occurences of _x in _l with A
subst(_,[],_,[]).
subst(_x,[_x|_l],_A,[_A|_M]) :- !, subst(_x,_l,_A,_M).
subst(_x,[_y|_l],_A,[_y|_M]) :- subst(_x,_l,_A,_M).
/*
% maplist(P,_l,M) : construct new list M by applying the boolean predicate P to all elements in _l
maplist(_,[],[]).
maplist(_P,[_x|_l],[_y|_M]) :-
        _Q =.. [_P,_x,_y],
        mycall(_Q),
        maplist(_P,_l,_M).

*/

  /** apply all ct's **/

apply_ctlist([]) :- !.
apply_ctlist([_head|_tail]) :-
        apply_ct(_head),
        apply_ctlist(_tail).

apply_ct(_ct) :-
        ct_action_filter(  _ct, _pre, _post, _preNE, _postNE), % todo: may be cached when computing dependencies 
        check_apply(            _pre, _post, _preNE, _postNE), 
        !,
        write('applying '),write(_ct),nl;
        true.                                                    % make sure everything is applied



/** apply ct if all preconditions are satified */       
check_apply(_pre, _post, _preNE, _postNE) :-     
        apply_pre( _pre, _preNE),                % if all preconditions satisfied       
        apply_post(_post, _postNE).              % then apply postconditions            % todo ??? : NE

/** check if all preconditions can be applied on fact base **/
apply_pre([],_).
apply_pre([_H|_T],_NE) :-
        mycall(_H),             % if call succeeds, unification
        checkInequalities(_NE), % and variable binding respects Inequalities
        apply_pre(_T,_NE).

/** apply ct by asserting/retracting all postconditions */
apply_post([],_postNE).
apply_post([not(_head)| _tail], _postNE) :-
        (myretract(_head); true),
        apply_post(_tail, _postNE).
apply_post([_head| _tail], _postNE) :- % head should already be unified
        _head \= not(_x),
        (myassert(_head);true),
        apply_post(_tail, _postNE).

/***************** test apply all ct's *****************/

:- dynamic ct_pre/1.
:- dynamic ct_post/1.
:- dynamic ct_post_save/1.

sim_apply_ct(_ct, _aPre, _aPost) :-
        ct_action_filter(  _ct, _pre, _post, _preNE, _postNE), % todo: may be cached when computing dependencies 
        sim_apply_pre(_pre,_preNE),
        sim_apply_post(_post,_postNE),
        !,
        findall(_x,ct_pre(_x) ,_aPre),
        findall(_y,ct_post(_y),_aPost),
        !,
        findall(_v,delete_tmp(_v),_trash).

delete_tmp(_deleted) :-
        (retract(ct_pre(_deleted));true),
        (retract(ct_post(_deleted));true).


/** check if all preconditions can be applied on fact base **/
sim_apply_pre([],_).
sim_apply_pre([_H|_T],_NE) :-
        (
                (mycall(_H),not(ct_post_save(not(_H))));  % if fact is already present and was not retracted by former ct
                ct_post_save(_H)                        % or fact was already asserted by former ct
        ),              
        checkInequalities(_NE), % and variable binding respects Inequalities
        (myassert(ct_pre(_H));true),
        sim_apply_pre(_T,_NE).

/** apply ct by asserting/retracting all postconditions */
sim_apply_post([],_).
sim_apply_post([not(_head)| _tail], _postNE) :-
        %(myretract(_head); true),
        (myassert(ct_post(not(_head)));true),
        (myassert(ct_post_save(not(_head)));true),
        sim_apply_post(_tail, _postNE).
sim_apply_post([_head| _tail], _postNE) :- 
        _head \= not(_x),
        %(myassert(_head);true),
        (myassert(ct_post(_head));true),
        (myassert(ct_post_save(_head));true),
        sim_apply_post(_tail, _postNE).

/** like apply, but always true, even if precondition isn't satified */
sim_unify_pre([],_).
sim_unify_pre([_H|_T],_NE) :-
        (
                (mycall(_H),not(ct_post_save(not(_H))));  % if fact is already present and was not retracted by former ct
                ct_post_save(_H);                       % or fact was already asserted by former ct
                true                                    % unify as much as possible, don't care if precondition is realy satified               
        ),              

%               (mycall(_H);true) % like sim_apply_pre, but precondition must only unify and must not be satified
        checkInequalities(_NE), % and variable binding respects Inequalities
        (myassert(ct_pre(_H));true),
        sim_unify_pre(_T,_NE).


sim_unify_ct(_ct, _aPre, _aPost) :-
        ct_action_filter(  _ct, _pre, _post, _preNE, _postNE), % todo: may be cached when computing dependencies 
        sim_unify_pre(_pre,_preNE),     
        sim_apply_post(_post,_postNE),
        !,
        findall(_x,ct_pre(_x) ,_aPre),
        findall(_y,ct_post(_y),_aPost),
        !,
        findall(_v,delete_tmp(_v),_trash).


                

/**************** Merging Pre&Post conditions for Composite CT **************/

:- dynamic ct/3.

/*
apply_merged_ctlist(_CTList) :-
        merge_ctlist(_CTList, _Pre, _Post),
        assert(ct(merged, _Pre, _Post)),
        apply_ct(merged),
        retract(ct(merged, _, _)).
*/
        

% creates a new ct _newName out of merged pre and post conditions of all ct's in _CTList
merge_ct(_CTList, _mergedPre, _mergedPost) :- %_mergedPre, _mergedPost) :-
        merge_ctlist([],[],_CTList,_mergedPre,_mergedPost),
        %assert(ct(_newName, _mergedPre, _mergedPost)),
        !,
        findall(_x,retract(ct_post_save(_x)),_trash).


merge_ctlist(_mergedPre, _mergedPost, [], _mergedPre, _mergedPost). % copy results when finished
merge_ctlist(_mergedPre, _mergedPost, [_head|_tail], _newMergedPre, _newMergedPost) :-
        !,
%       ct(_head, _pre, _post), % get pre & post condition of next ct % todo : hier kein filter (NE nicht nötig) ???
        sim_unify_ct(_head, _pre, _post), % unify variables with constants and simulate apply through ct_post_save/1 predicate
%       sim_apply_ct(_head, _pre, _post), % unify variables with constants and simulate apply through ct_post_save/1 predicate

        merge_pre(_mergedPre, _mergedPost, _pre,  _newMergedPre2),
        merge_post(_mergedPost           , _post, _newMergedPost2),
        %list2set(_newMergedPre2 , _newMergedPre3), % problem, da hier unifiziert wird
        %list2set(_newMergedPost2, _newMergedPost3),% problem, da hier unifiziert wird
        merge_ctlist(_newMergedPre2, _newMergedPost2, _tail, _newMergedPre, _newMergedPost).


/* merge_pre(Pre1,Post1,Pre2,newPre) merges Pre1 and (Pre2/Post1) with / dependend on myequals(_x,Y) */
merge_pre(_mergedPre, _mergedPost, [], _mergedPre).                       % _newMergedPre = _mergedPre
merge_pre(_mergedPre, _mergedPost, [_head|_tail], _newMergedPre) :-
        
%       member(_x, _mergedPost), condition_exists(_x,_head),                      % if _head is in _mergedPost ignore
        (member(_x, _mergedPost);member(_x, _mergedPre)),condition_exists(_x,_head),              % if _head is in _mergedPost delete, if in _mergedPre ignore, make copy of head to avoid recursive unification
        !,
        merge_pre(_mergedPre, _mergedPost, _tail, _newMergedPre).
merge_pre(_mergedPre, _mergedPost, [_head|_tail], _newMergedPre) :-
        !,
        merge_pre([_head|_mergedPre], _mergedPost, _tail, _newMergedPre). % otherwise add to _mergedPre

/* merge_post(Post1, Post2, newPost) merges Post1 and (Post2/Post1) with / dependend on myequals(_x,Y) */
merge_post(_mergedPost, _post, _newMergedPost) :-
        merge_pre(_mergedPost,_mergedPost,_post,_newMergedPost).          % add everyting to post, that is not already included



% Problem: == zu stark
%           = zu schwach und Problem, daß Variablen automatisch Instanziert werden, obwohl man unifikation nur testen will. 
condition_exists(_x,Y) :-
        %_x = Y. % loops bei unifikation vermeiden durch occurs check
                                
        %copy(_x,T1),
        copyVariable(Y,T2),
        _x = T2.
        
:- dynamic copyterm/1.

copyVariable(_x,Y) :-
        myassert(copyterm(_x)),
        myretract(copyterm(Y)).




/*********** testing stuff *************
unify(_x,Y) :-
        copy(_x,T1),
        copy(Y,T2),
        T1=T2.

copy(A,B) :-
        append([],[A],_l),
        member(B,_l).

myequals(_x,Y) :- 
        myvars(_x,L1),
        !,
        _x = Y,
        myvars(_x,L2),
        L1 = L2.

myvars([],_l) :- !.
myvars([H|T], L1) :-
        !,
        myvars2(H, L2),
        append(L1,L2,_l),
        myvars(T,_l).

myvars2(Term, Vars) :-
        Term =.. _l,
        findall(_x,(member(_x,_l),var(_x)),Vars).

%myequals(_x,Y) :- 
%       !, 
%       Y == _x.
*/


trans_depends(_x,_y) :-
        depends(_x, _y);
        !,
        depends(_x, _t),
        depends(_t, _y).

conflict(_x, _y) :-
        trans_depends(_x,_y),
        trans_depends(_y,_x),
        _x \= _y.

circle_list(_l) :-
        findall(_x, conflict(_x,_y), _l).
%       setof(_z,member(_z,l2),_l).



conflict_list(_l) :-
        !,
        findall( (_x,_y), conflict(_x,_y), _l).
%       symmetric_filter(_l2, _l).

symmetric_filter([],_).
symmetric_filter([(_x,_y)|_tail],_l) :-
        (member((_y,_x),_l);member((_x,_y),_l)),
        !,
        symmetric_filter(_tail,_l).
symmetric_filter([(_x,_y)|_tail],_l) :-
        !,
        symmetric_filter(_tail,[(_x,_y)|_l]).

        

/*************** Transformer Data **************/



ct(    ai,
       [field(_v,_c)],
       [method('get&_v',_c),method(set(_v),_c),readfield(_v,'get&_v'),writefield(_v, set(_v))]).

ct(    ac,
       [not(equals(_m,'get&_v')), method('get&_v',_c),readfield(_v,_m)],%,         writefield(_x,_y)],
       [not(readfield(_v, _m)) , invoke('get&_v',_m)]).


ct(    ci,
       [not(equals(_v, cnt(_v))),field(_v,_c)],%              ,invoke(_i,_m)],
       [field(cnt(_v),_c)]).


ct(    cc,
       [readfield(_v, _m), field(cnt(_v),_c), method(_m, _c)],
       [writefield(cnt(_v), _m)]).


:- dynamic writefield/2.
:- dynamic invoke/2.
:- dynamic class/1.
:- dynamic field/2.
:- dynamic method/2.
:- dynamic readfield/2.

%:- consult('data.pl').



/**************** Needed for compability with jwam - BinProlog **************/

mynot(_x)       :- findall(_elem,_x,_shouldBeEmpty), _shouldBeEmpty=[].
myassert(_x)    :- mynot(mycall(_x)),assert(_x). % assert only if not already exists
myretract(_x)   :- retract(_x).   
mycall(not(_x))   :- mynot(mycall(_x)).
mycall(_x)      :- _x \= not(_y),metacall(_x).



% !!! Problem in jwam: ?- human(uwe)==human(uwe) -> false




/**************** Needed for compability with eclipse **************

myassert(_x)    :- assert(_x).
myretract(_x)   :- retract(_x).
mycall(not(_x)) :- not(call(_x)).
mycall(_x)      :- not(_x = not(Y)),call(_x).

*/

/** Test predicates */

%apply_post([class(my),field(a,my),method(foo,my),readfield(a,foo)],[]).

% ?- ct_filter(_name, _pre, _post, _nepre, _nepost).
% ?- depends(_ct1,_ct2,_condition).
% ?- topo_sort(_sorted).

