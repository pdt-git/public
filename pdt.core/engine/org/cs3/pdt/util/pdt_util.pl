:-module(pdt_util,[
	pdt_call_cleanup/2,
	pdt_maybe/1,
	pdt_file_spec/2
]).

:-use_module(library('/org/cs3/pdt/util/pdt_util_io')).

/*
as the implementation of the sys pred call_cleanup/2 seems to be
broken, here is our own impl.
*/

pdt_call_cleanup(Goal,Cleanup):-
    catch(Goal,E,true),!,
    Cleanup,
    (	nonvar(E)
    ->	throw(E)
    ;	true
    ).
pdt_call_cleanup(_,Cleanup):-    
    Cleanup,
    fail.

/*
pdt_maybe(+Goal)

tries to call goal, catching all exceptions.

This predicate does always succeed.
*/
pdt_maybe(Goal):-
%	catch(		
		(Goal;true).%,
%		E,
%		debugme(E)
%	).
debugme(E):-
    pretty_print(current_output,'',E).
/*
file_spec(+FileSpec, -Abs)

pdt "standard" procedure for resolving file specifications
to absolute file names.

*/		
pdt_file_spec(FileSpec, Abs):-
	absolute_file_name(FileSpec,[extensions(['.pl','.ct','']),access(read)],Abs).