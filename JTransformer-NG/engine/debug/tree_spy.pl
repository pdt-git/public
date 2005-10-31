% Author:
% Date: 28.01.2003

:- dynamic spy_history/1.

spy_history([]).


tree_spy(type(_kind, _id, _dim)) :-
    format('type: ~a~a~n',[_kind,_dim]),
    tree_spy(_id).

tree_spy([_h|_t]) :-
    write('choose element number: \n'),
    get_char(_c),
    get_char(_),
    number_chars(_num, [_c]),
    nth0(_num,[_h|_t], _elem),
    tree_spy(_elem).


tree_spy(_id) :-
    not(getTerm(_id,_term)),
    !,
    format('no tree node: ~a~n', [_id]),
    next(b,_id,_term).

tree_spy(_id) :-
    spy_history(_oldList),
    retract(spy_history(_oldList)),
    assert(spy_history([_id| _oldList])),
    getTerm(_id,_term),
    write('0-9: switch to arg 0-9, a: print all children, p: parent, e or q: quit, h: help, v: view java source code\n'),
    write('   ______________________________________________________________________\n'),
    printTerm(_term),
    get_char(_c),
    get_char(_),
    next(_c, _id,_term).
    
    
%next('\'000\'',_,_).
%next('000',_,_).
next(e,_,_).
next(q,_,_).

next(b,_id,_term) :-
    not(spy_history([_h|_t])),
    !.

next(b,_id,_term) :-
    spy_history([_h|[_back|_t]]),
    retract(spy_history([_h|[_back| _t]])),
    assert(spy_history(_t)),
    tree_spy(_back).

next(a,_id,_term) :-
    _term =.. [_| [_id| _list]],
    printTermList(_list),
    tree_spy(_id).


    
next(v, _id, _term) :-
    write('\n'),
    gen_tree(_id),
    tree_spy(_id).
    
next(p, _id, _term) :-
    next('1', _id, _term).

next('\n',_id,_term) :-
    get_char(_c),
    get_char(_),
    next(_c, _id,_term).

next(_num_char,_id,_term) :-
    catch((
	    char_code(_num_char,_ascii),
	    _ascii > 47,
	    _ascii < 58,
	    plus(_num, 47,_ascii),
	%    succ(_num,_succ),
	    arg(_num,_term,_arg),
	    tree_spy(_arg)),
	    _,tree_spy(_id)).

next(_,_id,_) :-
    write('0-9: switch to arg 0-9, a: print all children, p: parent, \ne or q: quit, h: help, v: view java source code\n'),
    tree_spy(_id).

%next(_c,_id,_term):-
%    next(h,_id,_term).
printTerm(_term) :-
    retractT(output_to_file),
    term_to_atom(_term,_atom),
    functor(_term, _,_arity),
    succ(_arity_minus_1,_arity),
    printAlign,
    format('term: ~a: edges: ~a~n',[_atom,_arity_minus_1]).


printTermList([]).
printTermList([[_h |_ht]| _t]) :-
    indent,
    printTermList([_h | _ht]),
    undent,
    printTermList(_t).

printTermList([_h| _t]) :-
    (
      (
         getTerm(_h, _term),
         printTerm(_term)
      );
    true
    ),
    printTermList(_t).

