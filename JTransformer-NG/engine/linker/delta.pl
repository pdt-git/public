/**
 * Module to optionally track operations on the factbase.
 *
 * All assert/retract operations must be wrapped exec_with_delta(Operation,Term)
 * predicate.
 */
:- module(delta, 
          [
/* executes a predicate delta enabled */
           exec_with_delta/2,
/* asserts the argument delta enabled */
           assert_with_delta/1,
/* retracts the argument delta enabled */
           retract_with_delta/1,
/* query the for tracked delta information */
           query/2,
/* clear all tracked delta information */
           clear/0,
/* activate tracking */
           activate/0,
/* deactivate tracking */
           deactivate/0,
/* write delta information to disk */
           to_disk/1,
/* check if the delta tracking is activated */
           activated/0]).

:- dynamic create_delta_data/0.

activated :-
	create_delta_data.

activate :-
    not(create_delta_data),
    assert(create_delta_data),
    !.
activate.

deactivate :-
    retractall(create_delta_data),
    !.

clear :-
    recorded(pef_delta, _,Ref),
    erase(Ref),
    fail.
clear.

/**
 * exec_with_delta(+Pred,+Term)
 *
 * executes the predicate Pred/1 with Term 
 * as a parameter.
 * If delta generation is activate for each
 * tuple a record delta(Pred,Term) is appended
 * for the key "pef_delta".
 * 
 */
exec_with_delta(Kind,Term):-
    create_delta_data,
    !,
    recordz(pef_delta, delta(Kind,Term)),
    Wrapped =.. [Kind,Term],
    user:call(Wrapped).
exec_with_delta(Kind,Term):-
    Wrapped =.. [Kind,Term],
    user:call(Wrapped).
	
assert_with_delta(Term):-
	exec_with_delta(assert,Term).

retract_with_delta(Term):-
	exec_with_delta(retract,Term).	
	
query(Kind,Term):-
    recorded(pef_delta, delta(Kind,Term)).
    
/**
 * to_disk(+File)
 *
 * write pef_delta/2 fact to the file File.
 * 
 * e.g. pef_delta(assert, classDefT(1,2,3,[])).
 */
to_disk(File) :-
    open(File, write, Stream, []),
	set_stream(Stream, encoding(utf8)),
	write(Stream, 'encoding(utf8).'),
	nl(Stream),
	forall(query(Kind,Term), format(Stream, 'pef_delta(~w,~w).~n', [Kind,Term])),
	close(Stream).