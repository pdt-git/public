%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
:- dynamic output_to_memory/3.
:- dynamic output_to_memory_key/1.


/*
outdir(A):-
    project_option(_,output_project(A)).

%outdir('out_faj/').
%

set_outdir(Dir):-
	findall(Project,(
	          project_option(Project,output_project(_)),
			  retractall(project_option(Project,output_project(_))), 
		      assert(project_option(Project,output_project(Dir)))
		      ),_).

*/
output_to_file.

toggle_out :-
    output_to_file,
    print('output console'),
    !,
    retract(output_to_file).
toggle_out :-
    print('output file'),
    assert(output_to_file).

/**
 * open_printf_to_memory(Key) 
 *
 * Use the following pattern to ensure closing of your stream:
 *    call_cleanup(
 *   	  (	    
 *    	open_printf_to_memory(<key>),
 *	  ), close_printf_to_memory(<key>, Content)).
 *
 */
 
open_printf_to_memory(Key) :-
    output_to_memory(Key,Handle,Stream),
    retractall(output_to_memory(Key,_,_)),
    catch((
    close(Stream),
    free_memory_file(Handle)),
    Exception,true),
    format('EXCEPTION: catched Exception in open_printf_to_memory. Possible reason: trying to create an existing stream~nSTREAM: ~w ~n~w ~n', [Key,Exception]),
    fail.

open_printf_to_memory(Key) :-
    !, 
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream),
    asserta(output_to_memory(Key,Handle,Stream)),
	select_printf(Key).

/**
 * openUniqueMemoryStream(+Prefix,-Stream):-
 *
 * 
 */

open_unique_memory_stream(Prefix,Stream):-
    new_id(StreamID),     	
    concat(Prefix,StreamID,Stream),
   	open_printf_to_memory(Stream). 

/**
 * close_printf_to_memory(+Key,-Content) 
 *
 * Closes the current memory stream with key Key. The output is written into Content.
 * After closing, printf is set to the last memory stream that was opened before this one.
 * If Key does not exist, nothing will happen
 *
 */
my_call_cleanup(Goal,Cleanup):-
    catch(Goal,E,true),!,
    Cleanup,
    (	nonvar(E)
    ->	throw(E)
    ;	true
    ).
my_call_cleanup(_,Cleanup):-    
    Cleanup,
    fail.
    
close_printf_to_memory(Key,Content):-
	
    my_call_cleanup(
    	close_and_get_content(Key,Content),
    	delete_printf_to_memory(Key)
    ).
%close_printf_to_memory(_,_) :-
 %   throw('no memory file exists').
 

close_and_get_content(Key,Content):-
    output_to_memory(Key,Handle,Stream),
	close(Stream),
	memory_file_to_atom(Handle,Content).

delete_printf_to_memory(Key):-

    output_to_memory(Key,Handle,_),
    retractall(output_to_memory(Key,_,_)),
	free_memory_file(Handle),
	(  	output_to_memory_key(Key) 
    -> 	retract(output_to_memory_key(Key)),
		select_printf_last
	;  	true
	).

%close_printf_to_memory(Key,Content) :-
%	output_to_memory(Key,Handle,Stream),
%	!,
%    call_cleanup(      	
%    	memory_file_to_atom(Handle,Content),
%		do_close_printf_to_memory(Key,Stream,Handle)		
%	).
%
%do_close_printf_to_memory(Key,Stream,Handle):-
%    close(Stream),
%    free_memory_file(Handle),
%   	retract(output_to_memory(Key, Handle,Stream)),    
%    (  	output_to_memory_key(Key) 
%    -> 	retract(output_to_memory_key(Key)),
%		select_printf_last
%	;  	true
%	).
    

    

close_all_printf_to_memory:-
    close_all_printf_to_memory(_).
    
close_all_printf_to_memory(ContentTemp2):-
    not(output_to_memory(_,_,_)),
    ContentTemp2 = ''.    

close_all_printf_to_memory(Content) :-
    output_to_memory(Key,_,_),   
    !, 
    close_printf_to_memory(Key,ContentTemp),
    close_all_printf_to_memory(ContentTemp2),
    concat(ContentTemp,ContentTemp2,Content).

/**
 * select_printf(+Key)
 *
 * Select current memory stream.
 */

select_printf(Key) :-
	retractall(output_to_memory_key(_)),
	assert(output_to_memory_key(Key)).

select_printf_last :-
	output_to_memory(LastKey,_,_),
	select_printf(LastKey).
	
select_printf_last.

test(memory_file) :-
    open_printf_to_memory(testkey),
    printf(asdf),
    printf(asdf),
    close_printf_to_memory(testkey,Content),
    Content = asdfasdf.

printf(_format, _args) :-
    output_to_memory(_,_,_stream),
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
  current_prolog_flag(windows,_T) -> 
  set_prolog_flag(tty_control,false). 

:- disable_tty_control.