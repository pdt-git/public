:-module(pdt_util,[
	pdt_call_cleanup/2,
	pdt_maybe/1,
	pdt_file_spec/2,
	pdt_member/2,
	pdt_chop_before/3,
	pdt_chop_after/3
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
	absolute_file_name(FileSpec,[file_errors(fail),extensions(['.pl','.ct','']),access(read)],Abs).

/*
pdt_member(?Member,+List):-
    true if Member is an element of List.
    
    Redefines builtin member/2 to handle lists with an unbound tail.
    Like member/2 this predicate will bind unbound elements of the list.
    Unlike member/2, this predicate will NOT bind an unbound tail.
*/	
pdt_member(_,Var):-
    var(Var),!,fail.
pdt_member(M,[M|_]).
pdt_member(M,[_|T]):-
    pdt_member(M,T).
    
    
% pdt_chop_before(+Elm,+Elms,+Suffix)

% Elms should be an orderd list.
% Suffix will unified with the first suffix of Elms
% whos elements are equal or greater than Elm.
pdt_chop_before(Elm,[Elm|Elms],[Elm|Elms]):-
	!.
pdt_chop_before(Elm,[Head|NextElms],Elms):-
    Head@<Elm,!,
    pdt_chop_before(Elm,NextElms,Elms).
pdt_chop_before(_,Elms,Elms).

% pdt_chop_after(+Elm,+Elms,+Suffix)

% Elms should be an orderd list.
% Suffix will unified with the first suffix of Elms
% whos elements are strictly greater than Elm.
pdt_chop_after(Elm,[Head|NextElms],Elms):-
    Head@=<Elm,!,
    pdt_chop_after(Elm,NextElms,Elms).
pdt_chop_after(_,Elms,Elms).
        