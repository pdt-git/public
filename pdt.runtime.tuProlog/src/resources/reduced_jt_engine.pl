
/**
 * initLastID
 * adds the fact lastID(10000) if no lastID fact exists.
 *
 * Do not expose: internal predicate for new_id/1!
 */

:- dynamic lastID/1.


lastID(10000).

/*
initLastID :-
  lastID(_),
  !.
initLastID :-
  assert(lastID(10000)).
  
:- initLastID.
*/

/**
 * new_id(-Id)
 *
 * Binds Id to a unique number.
 * Throws already_bound_exception(Msg)
 * if Id is already bound.
 *
 */     

new_id(New) :-
        nonvar(New),
        New = [_|_],
        !,
        term_to_atom(New,Term),
        sformat(Msg,'new_id: variable is a list: ~a~n',Term),
        debugme,
        print(Msg),
        flush_output,
        throw(already_bound_exception(Msg)).
        
new_id(New) :-
        nonvar(New),
        !,
        sformat(Msg,'new_id: variable already bound: ~w~n',[New]),
        print(Msg),
        flush_output,
        throw(already_bound_exception(Msg)).

new_id(_New) :-
        findall(ID,lastID(ID),[H|[H2|T]]),
        !,
        sformat(Msg,'more than one lastID fact: ~w~n',[[H|[H2|T]]]),
        print(Msg),
        flush_output,
        throw(more_than_one_fact_exception(Msg)).

new_id(_New) :-
    lastID(_last),
    sum(_last, 1, _New),
    retract(lastID(_last)),
    assert(lastID(_New)).
