% Author: Tobias
% Date: 14.11.2002

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

/*
interpret([_factname | _atomlist]) :-
   exchangeLocalWithGlobalIds(_atomlist, [ID|Rest]),
    Term =.. [_factname | [ID|Rest]],
% 	checkExistance(ID,Term),
    assert1T(Term).
*/

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

/*
Sts: Seem totally obsolete, removing them does not generate more errors then
having them ;-) Also, caused a bug (the first clase was added to circumvent it)
inTe(importT(_,_, lId(X))) :-
		local2FQN(X, FQN),
		not(globalIds(FQN, _),
		new_id(X),
		assert(packageT(X, FQN)),
		fail.

inTe(importT(_,_,Fqn)) :-
    not(fqn(_) = Fqn),
    not(globalIds(Fqn, _)),
		not(lId(_) = Fqn),
    new_id(X),
    assert(packageT(X, Fqn)),
    fail.
*/

%inTe(importT(_, _, Fqn)) :-
%    globalIds(Fqn, _),
%    !.    

inTe(packageT(_,Name)) :-
    packageT(_,Name),
    !.

inTe(':'(abba,Term)) :-
	':'(abba,Term),
	!.
    
%inTe(packageT(_,_name)) :-
%    new_id(_id),
%    assert(packageT(_id,_name)),
%    !.

inTe(_term) :-
  inTe(_term,_translated),
 	checkExistance(_translated),
 	_translated =.. AsList,
	not(member(lId(_), AsList)),
	!,
	assert(_translated).

inTe(_term) :-
	inTe(_term, _translated),
	checkExistance(_translated),
	_translated =.. AsList,
	member(lId(LID), AsList),
	!,
	printf('Local ID ~a is in the term', [LID]),
	print(_translated),
	printf('~n'),
	fail.


inTe(_term, _Translated) :-
    _term =.. [_name | _args],
    exchangeLocalWithGlobalIds(_args, _translatedArgs),
    _Translated =.. [_name | _translatedArgs].

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


globalFqn(_fqn, _fieldName,_globalid) :-
    globalIds(_fqn, _fieldName,_globalid),
    !.

globalFqn(_fqn, _fieldName,_globalid) :-
    new_id(_globalid),
    _term =..[globalIds, _fqn, _fieldName,_globalid],
%    add_to_new(_fqn),
    assert(_term).

/*
  globalFqn(+Name,-Id)
  
  Creates new id for for Name, if no globalIds(Name,..) exists.
  Otherwise globalIds(Name, Id) is evaluated.
*/

globalFqn(_fqn, _globalid) :-
    globalIds(_fqn, _globalid),
    !.

globalFqn(_fqn, _globalid) :-
    new_id(_globalid),
    _term =..[globalIds, _fqn, _globalid],
    assert(_term).
%    add_to_new(_fqn).
    

/*
add_to_new(_fqn):-
    unresolved_types(_fqn),
	packageT(_,_fqn),
	!.

add_to_new(_fqn):-
    unresolved_types(_fqn),
    !.

add_to_new(_fqn):-
	packageT(_,_fqn),
	!.
	    
add_to_new(_fqn):-
    assert(unresolved_types(_fqn)).
*/
/*
consultIfNewWriteCache([_H|_T]) :-
    _term =..[consultIfNew, [_H |_T]],
    write_cache_clause(_term),
    close_cache_file,
    consultIfNew([_H| _T]).
    
consultIfNew([]).
consultIfNew([_H|_T]) :-
    consultIfNew(_H),
    consultIfNew(_T).
consultIfNew(_filename) :-
    appendDir(_filename, _dirname),
    not(consulted(_filename)),
    !,
    assert(consulted(_filename)),
    consult(_dirname).
    %consultCacheOrOriginal(_filename).
consultIfNew(_).

consultCacheOrOriginal(_filename) :-
    removeExt(_filename, _noExt),
    concat_atom([_noExt,'.cache.pl'], _cacheFilename),
    exists_file(_cacheFilename),
    consult(_cacheFilename),!.

consultCacheOrOriginal(_filename) :-
    consult(_filename).
*/


removeExt(_filename, _NoExt) :-
    atom_length(_filename, _size),
    plus(3, _sizeNoExt, _size),
    sub_atom(_filename, 0, _sizeNoExt, _,_NoExt).

open_cache_file :-
    open_cache_file('cache.pl').

open_cache_file(_name).
%    open(_name, write, _fileStream),
%    assert(cache_file(_fileStream)).

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
    assert(_term).


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
    assert(globalIds(Fqn,Id)).
	
addToGlobalIdsAndFacts(Fqn):-
    globalIds(Fqn,_),
    !.

addToGlobalIdsAndFacts(Fqn):-
    new_id(Id),
		printf('creating synthetic package '),
		print(Fqn),
		printf('~n'),
    assert(globalIds(Fqn,Id)),
    assert(packageT(Id,Fqn)).		

/**
 * remove_contained_global_ids(+ToplevelPath)
 *
 * Retracts all global ids of classes defined
 * in the toplevel specified by 'ToplevelPath'.
 */
    
remove_contained_global_ids(Path):-
	toplevelT(_, _, Path, Defs), 
	forall((member(M,Defs),classDefT(M,_,_,_)),
	       retractall(globalIds(_,M))).