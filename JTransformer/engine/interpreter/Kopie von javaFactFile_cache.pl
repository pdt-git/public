% Author: Tobias
% Date: 14.11.2002

:- multifile globalIds/2.
:- dynamic globalIds/2.
:- dynamic symtab/2.
:- multifile symtab/1.

:- dynamic cache_file/1.
:- multifile cache_file/1.

open_cache_file :-
    open_cache_file('cache.pl').
    
open_cache_file(_name) :-
    open(_name, write, _fileStream),
    assert(cache_file(_fileStream)).

close_cache_file :-
    cache_file(_fileStream),
    close(_fileStream),
    retract(cache_file(_fileStream)).


write_cache_clause(_term) :-
    cache_file(_fileStream),
    format(_fileStream, ':- ',[]),
    write_term(_fileStream, _term, [quoted(true)] ),
    format(_fileStream, '.~n',[]).

write_cache(_term) :-
    cache_file(_fileStream),
    write_term(_fileStream, _term, [quoted(true)] ),
    format(_fileStream, '.~n',[]).

interpret([_factname | _atomlist]) :-
    exchangeLocalWithGlobalIds(_atomlist, _rest),
    _term =.. [_factname | _rest],
    write_cache(_term),
    assert1T(_term).


exchangeLocalWithGlobalIds([], []) :- !.
exchangeLocalWithGlobalIds([symtab(_id) | _t], [_globalid | _rest]) :-
    symtab(_id, _fqn),
    !,
    globalSymtab(_fqn, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([type(class, symtab(_id), _dim) | _t], [_term | _rest]) :-
    symtab(_id, _fqn),
    !,
    globalSymtab(_fqn, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest),
    _term =.. [type, class, _globalid, _dim].

exchangeLocalWithGlobalIds([localId(_id) | _t], [_globalid | _rest]) :-
    localSymtab(_id, _globalid),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([[_h | _ht] | _t], [_listglobals | _rest]) :-
    exchangeLocalWithGlobalIds([_h|_ht], _listglobals),
    exchangeLocalWithGlobalIds(_t, _rest).

exchangeLocalWithGlobalIds([_id | _t], [_id | _rest]) :-
    exchangeLocalWithGlobalIds(_t, _rest).

localSymtab(_id, _globalid) :-
    symtab(_id, _globalid),!.

localSymtab(_id, _globalid) :-
    newID(_globalid),
    _term =..[symtab, _id, _globalid],
    assert(_term).

globalSymtab(_fqn, _globalid) :-
    globalIds(_fqn, _globalid),!.
    
globalSymtab(_fqn, _globalid) :-
    newID(_globalid),
    _term =..[globalIds, _fqn, _globalid],
    write_cache(_term),
    assert(_term).

consultIfNewWriteCache([_H|_T]) :-
    _term =..[consultIfNew, [_H |_T]],
    write_cache_clause(_term),
    close_cache_file,
    consultIfNew([_H| _T]).
    
consultIfNew([_H|_T]) :-
    consultIfNew(_H),
    consultIfNew(_T).
consultIfNew(_filename) :-
    not(consulted(_filename)),
    !,
    assert(consulted(_filename)),
    consultCacheOrOriginal(_filename).
consultIfNew(_).

consultCacheOrOriginal(_filename) :-
    removeExt(_filename, _noExt),
    concat_atom([_noExt,'.cache.pl'], _cacheFilename),
    exists_file(_cacheFilename),
    consult(_cacheFilename),!.

consultCacheOrOriginal(_filename) :-
    consult(_filename).

removeExt(_filename, _NoExt) :-
    atom_length(_filename, _size),
    plus(3, _sizeNoExt, _size),
    sub_atom(_filename, 0, _sizeNoExt, _,_NoExt).

