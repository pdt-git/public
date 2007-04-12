:- use_module(delta).
% Author: Tobias
% Date: 14.11.2002
/**
 * Usage:
 *
 * 1. for classes or package generate:
 *     local2FQN(lId(<MyLocalID>), fqn('<full qualified class or package name>')). 
 *    for fields:
 *     local2FQN(lId(<MyLocalID>), fqn('<full qualified class name>', '<field name>')). 
 *    for method:
 *     local2FQN(lId(<MyLocalID>), fqn('<full qualified class name>', '<method name>', ['<fqn param type 1>', ...])). 
 * 
 * 2. Now add your pefs / facts via inTe/1 (interprete Term), e.g.: 
 * 
 *     :- inTe(classDefT(lId(<classId>),lId(<classId | packageId>), <name>, [lId(<body id | null>), ...]).
 *
 * 3. clean local symbol table (localFQN, symbol)
 *     :- retractLocalSymtab.
 *
 * 4. retrieve the FQNs of the unresolved class names (via backtracking):
 *     :- unresolved_types(-UnresolvedTypeName)
 */


/**
 * use_reverse_indexes
 *
 * If this fact exists no reverse
 * indexes are created.
 */
:- dynamic use_reverse_indexes/0.

/**
 * deactivate_reverse_indexes
 *
 * Deactivate generation of reverse indexes.
 */
deactivate_reverse_indexes :-
	retractall(use_reverse_indexes).

/**
 * deactivate_reverse_indexes
 *
 * Activate generation of reverse indexes.
 */
activate_reverse_indexes :-
	retractall(use_reverse_indexes),
	assert(use_reverse_indexes).

/**
 * globalIds(+full qualfied type name, +Id)
 *
 * Global (process-wide) mapping
 * full qualified type name and id.
 * Generate by the PEF linking (this file).
 */
:- multifile globalIds/2.
:- dynamic globalIds/2.

/**
 * ri_globalIds(+Id, +full qualfied type name)
 *
 * reverse index for globalIds.
 */
:- multifile ri_globalIds/2.
:- dynamic ri_globalIds/2.

/**
 * globalIds(+full qualfied type name, +field name, +Id)
 */
:- multifile globalIds/3.
:- dynamic globalIds/3.
/**
 * globalIds(+full qualfied type name, +method name, +parameter type list, +Id)
 */
:- multifile globalIds/4.
:- dynamic globalIds/4.


/**
 * symtab(+Id, +fullQualifiedName)
 * 
 * Contains the local symbol table for a file/class.
 */
:- dynamic symtab/2.
:- dynamic errors_in_java_code/0.
:- dynamic ignore_unresolved_type/1.

:- dynamic local2FQN/2.
:- multifile local2FQN/2.
%:- dynamic unresolved_types/1.

:- dynamic cache_file/1.
:- multifile cache_file/1.



checkExistance(Term) :-
    Term =.. [Functor | [ID | _]],
    tree(ID,_,_),
    treeSignature(Functor,_),
    getTerm(ID, ExistingTerm),
    term_to_atom(Term,TermAtom),
    term_to_atom(ExistingTerm,ExistingTermAtom),
    sformat(Msg,'the id ~a of term ~a already exists in fact ~a~n', 
    [ID,TermAtom,ExistingTermAtom]),
    throw(Msg).
    
checkExistance(_).


inTeArray(Term) :-
	forall(arg(_, Term, Value),
	       call(Value)).

inTe(packageT(_,Name)) :-
    packageT(_,Name),
    !.

inTe(':'(abba,Term)) :-
	':'(abba,Term),
	!.
    
inTe(Term) :-
  inTe(Term,Translated),
 	checkExistance(Translated),
 	Translated =.. AsList,
	not(member(lId(_), AsList)),
	!,
	exec_with_delta(assert1T,Translated),
	assertTreeReverseIndexes(Translated).

inTe(Term) :-
	inTe(Term, Translated),
	checkExistance(Translated),
	Translated =.. AsList,
	member(lId(LID), AsList),
	!,
	printf('Local ID ~a is in the term', [LID]),
	print(Translated),
	printf('~n'),
	fail.


inTe(Term, Translated) :-
    Term =.. [_name | _args],
    exchangeLocalWithGlobalIds(_args, _translatedArgs),
    Translated =.. [_name | _translatedArgs].

exchangeLocalWithGlobalIds([], []) :- !.
exchangeLocalWithGlobalIds([_var | _t], [_var | _rest]) :-
    var(_var),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([fqn(_fqn) | _t], [_globalid | _rest]) :-
    !,
    globalFqn(_fqn, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([fqn(_fqn,_fieldName) | _t], [_globalid | _rest]) :-
    !,
    globalFqn(_fqn, _fieldName, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([fqn(_fqn,_methName,_paramTypes) | _t], [_globalid | _rest]) :-
    !,
    globalFqn(_fqn, _methName, _paramTypes, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([type(class, fqn(_fqn), _dim) | _t], [_term | _rest]) :-
    !,
    globalFqn(_fqn, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest),
    _term =.. [type, class, _globalid, _dim].

/*
 * StS: local2FQN replaces the local ID by an FQN
 */
 
 
exchangeLocalWithGlobalIds([lId(_id) | _t], [_globalid | _rest]) :-
    local2FQN(lId(_id), FQN),
    !,
    exchangeLocalWithGlobalIds([FQN | _t], [_globalid | _rest]).
    
/* END EDIT */

exchangeLocalWithGlobalIds([lId(_id) | _t], [_globalid | _rest]) :-
    localSymtab(_id, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).


exchangeLocalWithGlobalIds([lId(_id) | _t], [_globalid | _rest]) :-
    localSymtab(_id, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).


exchangeLocalWithGlobalIds([[_h | _ht] | _t], [_listglobals | _rest]) :-
    !,
    exchangeLocalWithGlobalIds([_h|_ht], _listglobals),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([_id | _t], [_translatedTerm | _rest]) :-
    nonvar(_id),
    _id  =.. [_hterm | _tterm],
    _tterm \= [],
    exchangeLocalWithGlobalIds(_tterm, _ttranslated),
    _translatedTerm =.. [_hterm | _ttranslated],
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([_id | _t], [_id | _rest]) :-
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([[_h | _ht] | _t], [_listglobals | _rest]) :-
    !,
    exchangeLocalWithGlobalIds([_h|_ht], _listglobals),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([_id | _t], [_id | _rest]) :-
    exchangeLocalWithGlobalIds(_t, _rest).


globalFqn(_fqn, _methName, _paramTypes, _globalid) :-
    globalIds(_fqn, _methName, _paramTypes, _globalid),
    !.

globalFqn(_fqn, _methName, _paramTypes, _globalid) :-
    new_id(_globalid),
    _term =..[globalIds, _fqn, _methName, _paramTypes, _globalid],
%    add_to_new(_fqn),
	assert(_term).
%    exec_with_delta(assert,_term).


globalFqn(_fqn, _fieldName,_globalid) :-
    globalIds(_fqn, _fieldName,_globalid),
    !.

globalFqn(_fqn, _fieldName,_globalid) :-
    new_id(_globalid),
    _term =..[globalIds, _fqn, _fieldName,_globalid],
%    add_to_new(_fqn),
	assert(_term).
%    exec_with_delta(assert,_term).

/*
  globalFqn(+Name,-Id)
  
  Creates new id for for Name, if no globalIds(Name,..) exists.
  Otherwise globalIds(Name, Id) is evaluated.
*/

globalFqn(_fqn, _globalid) :-
    globalIds(_fqn, _globalid),
    !.

globalFqn(Fqn, GlobalId) :-
    new_id(GlobalId),  
    assertGlobalIds(Fqn,GlobalId).
%    add_to_new(_fqn).
    


removeExt(Filename, NoExt) :-
    atom_length(Filename, Size),
    plus(3, SizeNoExt, Size),
    sub_atom(Filename, 0, SizeNoExt, _,NoExt).

open_cache_file :-
    open_cache_file('cache.pl').

open_cache_file(__name).

close_cache_file.
%    cache_file(_fileStream),
%    close(_fileStream),
%    retract(cache_file(_fileStream)).


write_cache_clause(_term) :-
    cache_file(_fileStream),
    format(_fileStream, ':- ',[]),
    write_term(_fileStream, _term, [quoted(true)] ),
    format(_fileStream, '.~n',[]).

write_cache(_term) :-
    cache_file(_fileStream),
    write_term(_fileStream, _term, [quoted(true)] ),
    format(_fileStream, '.~n',[]).
    
/**
 * unresolved_types(-UnresolvedTypeName)
 * 
 * will bind UnreslvedTypeName to
 * all types not defined in the current engine.
 */

unresolved_types(NAME) :- 
	globalIds(NAME, ID),
	not(classDefT(ID, _,_,_)),
	not(packageT(_, NAME)),
	not(atom_concat(_,'$1',NAME)),
	not(ignore_unresolved_type(NAME)).

	



localSymtab(_id, _globalid) :-
    symtab(_id, _globalid),!.

localSymtab(_id, _globalid) :-
    new_id(_globalid),
    _term =..[symtab, _id, _globalid],
    assert(_term).  % <- temporary, not delta relevant


retractLocalSymtab :-
	retractall(symtab(_,_)),
	retractall(local2FQN(_,_)).


type_dependency(null,Fqn):-
    !,
    addToGlobalIds(Fqn).

type_dependency(Pckg,Fqn):-
    addToGlobalIdsAndFacts(Pckg),
    addToGlobalIds(Fqn).
    
addToGlobalIds(Fqn):-
    globalIds(Fqn,_),
    !.

addToGlobalIds(Fqn):-
    new_id(Id),
    assertGlobalIds(Fqn,Id).
	
addToGlobalIdsAndFacts(Fqn):-
    globalIds(Fqn,_),
    !.

addToGlobalIdsAndFacts(Fqn):-
    new_id(Id),
		printf('creating synthetic package '),
		print(Fqn),
		printf('~n'),
    assertGlobalIds(Fqn,Id),
    exec_with_delta(assert,packageT(Id,Fqn)).

/**
 * remove_contained_global_ids(+ToplevelPath)
 *
 * Retracts all global ids of classes defined
 * in the toplevel specified by 'ToplevelPath'.
 */
    
remove_contained_global_ids(Path):-
	toplevelT(_, _, Path, Defs), 
	forall((member(M,Defs),classDefT(M,_,_,_)),
	       (retractall(globalIds(_,M)),
	       retractall(ri_globalIds(M,_))
	       )).	       

	       
assert_tree(Translated) :-
    exec_with_delta(assert,Translated),
	Translated =.. [Functor,Id | Args],
	ast_node_def('Java',Functor,[_|ArgDefs]),
	!,
	assert_reverse_indexes(assert,Functor,Id, Args, ArgDefs).
assert_tree(_Translated).

retract_tree(Translated) :-
	retract(Translated),
	Translated =.. [Functor,Id | Args],
	ast_node_def('Java',Functor,[_|ArgDefs]),
	!,
	assert_reverse_indexes(retract,Functor,Id, Args, ArgDefs).
retract_tree(_Translated).


assertTreeReverseIndexes(Translated) :-
	actionOnTreeReverseIndexes(assert,Translated).

retractTreeReverseIndexes(Translated) :-
	actionOnTreeReverseIndexes(retract,Translated).

addTreeReverseIndexes(Translated) :-
	actionOnTreeReverseIndexes(add,Translated).

deleteTreeReverseIndexes(Translated) :-
	actionOnTreeReverseIndexes(delete,Translated).

actionOnTreeReverseIndexes(Action,Translated) :-
	Translated =.. [Functor,Id | Args],
	ast_node_def('Java',Functor,[_|ArgDefs]),
	!,
	assert_reverse_indexes(Action,Functor,Id, Args, ArgDefs).
actionOnTreeReverseIndexes(_Action,_Translated).



/**
 * assert_reverse_indexes(+AssertAdd,+Id, +Args, ArgDefs)
 *
 * Creates a reverse index ri_kind for every
 * arg kind of type id and mult(1,1,no ).
 * See ast_node_def/3 for details on arg kinds.
 * If AssertAdd equals 'assert' the fact will be asserted
 * if it equals 'add' it will be added.
 */
 
assert_reverse_indexes(_AssertAdd,_Functor,_Id, __Args, __RestDefs):-
  \+ use_reverse_indexes,
  !.
  
assert_reverse_indexes(_AssertAdd,_Functor,_Id, [], __RestDefs).

assert_reverse_indexes(AssertAdd, Functor,Id, [Arg|Args],
	[ast_arg(Kind,  mult(1,1,no ), id,  _)|RestDefs]):-
    !,
    ri_functor_kind(Functor,Kind,RiKind),
    RiTerm =.. [RiKind,Arg,Id],
    
    exec_with_delta(AssertAdd,RiTerm),
	assert_reverse_indexes(AssertAdd,Functor,Id, Args, RestDefs).

assert_reverse_indexes(AssertAdd,Functor,Id, [_Arg|Args],
	[_Def|RestDefs]):-
	assert_reverse_indexes(AssertAdd,Functor,Id, Args, RestDefs).


assert_or_add(add,RiTerm) :-
	add(RiTerm).
assert_or_add(delete,RiTerm) :-
	delete(RiTerm).
assert_or_add(assert,RiTerm) :-
	assert(RiTerm).
assert_or_add(retract,RiTerm) :-
	retract(RiTerm).

/**
 * create_ast_arg_id_kind
 * 
 * First retract and then create ast_arg_id_kind
 * facts from the ast_node_def clauses.
 */
create_dynamic_directive_for_arg_id_kinds :-
	forall((
	   ast_node_def('Java',Functor,Defs),
      member(ast_arg(Kind,  mult(1,1,no ), id,  _),Defs),
      ri_functor_kind(Functor,Kind,Ri)),
	  dynamic(Ri/2)).

create_ri_functor_kind :-
    retractall(ri_functor_kind(_,_,_)),
	forall((
	    ast_node_def('Java',Functor,Defs),
        member(ast_arg(Kind,  mult(1,1,no ), id,  _),Defs),
        ri_functor_kind_functor(Functor,Kind,RiFunctor)
      ),
	  assert(ri_functor_kind(Functor,Kind,RiFunctor))).

ri_functor_kind_functor(Functor,Kind,RiFunctorKind):-
	  atom_concat('ri_', Functor,RiFunctor),
	  atom_concat(RiFunctor,'_',RiFunctorDash),
	  atom_concat(RiFunctorDash,Kind,RiFunctorKind).

:- create_ri_functor_kind.
:- create_dynamic_directive_for_arg_id_kinds.


assertGlobalIds(Fqn,Id):-
    assert(globalIds(Fqn,Id)),
    assert(ri_globalIds(Id,Fqn)).
