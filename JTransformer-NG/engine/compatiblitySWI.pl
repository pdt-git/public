:- consult(library(memfile)).
:- multifile test/1.

/**
 * SWI Kompability
 * specific for SWI Prolog
 */

:- dynamic outdir/1.
:- dynamic file_output/1.
:- dynamic output_to_file/0.
%:- dynamic output_to_tmp/1.
:- dynamic output_to_memory/2.

outdir('out_faj/').

set_outdir(Dir):-
    retractall(outdir(_)),
    assert(outdir(Dir)).

output_to_file.

toggle_out :-
    output_to_file,
    print('output console'),
    !,
    retract(output_to_file).
toggle_out :-
    print('output file'),
    assert(output_to_file).


open_printf_to_memory :-
%	output_to_memory(_,_),
%	throw('memory file still open').
    output_to_memory(Handle,Stream),
    retractall(output_to_memory(_,_)),
    catch((
    close(Stream),
    free_memory_file(Handle)),
    Exception,true),
    format('catched Exception in open_printf_to_memory: ~a~n', Exception),
    fail.


open_printf_to_memory :-
    !,
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream),
    assert(output_to_memory(Handle,Stream)).
	

close_printf_to_memory(Content) :-
    output_to_memory(Handle,Stream),
    !,
    close(Stream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle),
    retract(output_to_memory(Handle,Stream)).

close_printf_to_memory(_) :-
    throw('no memory file exists').



test(memory_file) :-
    open_printf_to_memory,
    printf(asdf),
    printf(asdf),
    close_printf_to_memory(Content),
    Content = asdfasdf.

printf(_format, _args) :-
    output_to_memory(_,_stream),
    !,
    format(_stream, _format, _args).

%printf(_format, _args) :-
%    output_to_tmp(_stream),
%    !,
%    format(_stream, _format, _args).

printf(_format, _args) :-
    output_to_file,
    file_output(_stream),
    !,
    format(_stream, _format, _args).

printf(_format, _args) :-
    current_output(_stream),
    format(_stream, _format, _args),
    flush_output.


printf(_format) :-
    printf(_format, []).
%    file_output(_stream),
%    current_output(_stream),
%    format(_stream, _format, []).

    

println :-
    printf('~n').

% only assert if fact does not exist
assert1(_x) :- not(call(_x)), assert(_x).

% variations, where assert/retract should always be true
assertT(_x) :- assert(_x).
assertT(_).
/*
	assert1T(+Term)
	Asserts term Term. If Term already
	exists it will not be asserted.
	The predicate is always true.
*/
assert1T(_x) :- assert1(_x).
assert1T(_).
retractT(_x) :- retract(_x).
retractT(_).

/**
	stringAppend(?Atom1, ?Atom2, Atom3)
	
	Atom3 forms the concatination of Atom1 and Atom2.
	At least two arguments must be instantiated.
	
	Mapped to atom_concat/3. (Needed for ISO-Prolog Compatibility).
*/

stringAppend(_str1, _str2, _Ret) :-
    atom_concat(_str1, _str2, _Ret).


test('stringAppend/3#1') :- stringAppend('','','').
test('stringAppend/3#2') :- stringAppend('a','','a').
test('stringAppend/3#3') :- stringAppend('','a','a').
test('stringAppend/3#4') :- stringAppend('a','b','ab').
test('stringAppend/3#4') :- stringAppend('uwe ','tarek bardey','uwe tarek bardey').

list2java(_l, _S) :-
    concat_atom(_l, ', ', _S).


mapPredicate(_, _ ,[] ,[]).
mapPredicate(_Pred, _Arg1 ,[_Arg2H | _Arg2T] ,[_RetH | _RetT]) :-
               _Q =.. [_Pred, _Arg1, _Arg2H, _RetH],
               call(_Q),
               mapPredicate(_Pred, _Arg1, _Arg2T, _RetT).

sum(_Int1, _Int2, _Int3) :- plus(_Int1, _Int2, _Int3).

int2string(_int, _string) :- swritef(_string, "%d", [_int]).

equals(_term1, _term2) :- _term1 = _term2.
nequals(_term1, _term2) :- _term1 \= _term2.

debugPrint(_str) :- writef(_str).

has_property(_pred,_prop,1) :- 
	predicate_property(_pred,_prop),
	!.
	
has_property(_pred,_prop,0).


/**
*/

open_print_to_memory :-
	output_to_memory(_,_),
	throw('memory file still open').

open_print_to_memory :-
    !,
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream),
    current_output(Out),
    assert(output_to_memory(Handle,Out)),
    set_output(Stream).

close_print_to_memory(Content) :-
    output_to_memory(Handle,Out),
    !,
    current_output(MemStream),
    close(MemStream),
    set_output(Out),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle),
    retract(output_to_memory(Handle,Out)).

close_print_to_memory(_) :-
    throw('no memory file exists').

/*
:- redefine_system_predicate(get_single_char(_)).

get_single_char(A) :-
    print(aha),
    system:get_single_char(A).
*/

/**
 * disable_tty_control
 *
 * Disables tty control char-wise read on the windows platform.
 */

disable_tty_control :- 
  current_prolog_flag(windows,T) -> 
  set_prolog_flag(tty_control,false). 

:- disable_tty_control.