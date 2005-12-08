:-module(fileref_anotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/anotate/pdt_anotator')).



term_pre_anotation_hook(Stack,_,InTerm,OutTerm):-
	file_refs(Stack,InTerm,OutTerm).

file_anotation_hook(_,_,Terms,InAnos,[references_files(Refs)|InAnos]):-
    collect_refs(Terms,Refs).


file_refs(Stack,InTerm,OutTerm):-
    pdt_strip_anotation(InTerm,Term,(Head,Tail)),
    find_file_refs(Term,Refs),   
    resolve_file_Refs(Refs,ResRefs),
    anotate_refered_files(Stack,ResRefs),
    pdt_splice_anotation(Term,([file_refs(ResRefs)|Head],Tail),OutTerm).

    
find_file_refs(:-[H,T],[H,T]).
find_file_refs(:-load_files([H,T],_),[H,T]).
find_file_refs(:-load_files(H,_),[H]):-
    \+ is_list(H).
find_file_refs(:-consult([H,T]),[H,T]).
find_file_refs(:-consult(H),[H]):-
    \+ is_list(H).
find_file_refs(:-ensure_loaded([H,T]),[H,T]).
find_file_refs(:-ensure_loaded(H),[H]):-
    \+ is_list(H).
find_file_refs(:-include(H),[H]):-
    \+ is_list(H).    
find_file_refs(:-use_module([H,T]),[H,T]).
find_file_refs(:-use_module(H),[H]):-
    \+ is_list(H).
find_file_refs(:-use_module([H,T],_),[H,T]).
find_file_refs(:-use_module(H),_,[H]):-
    \+ is_list(H).

resolve_file_Refs([],[]).
resolve_file_Refs([H|T],[RH|RT]):-
    pdt_file_spec(H,RH),
    resolve_file_Refs(T,RT).

anotate_refered_files(_,[]).
anotate_refered_files(Stack,[H|T]):-
%    the_debug(Stack,H),
    anotate_refered_file([H|Stack]),
%    this_debug(Stack),
    anotate_refered_files(Stack,T).

anotate_refered_file([H|Stack]):-
	member(H,Stack),!.    
anotate_refered_file(Stack):-
	ensure_anotated(Stack).    	

%the_debug([Cur|_],H):-
%    format("~w refers to ~w~n",[Cur,H]).
%
%this_debug([Cur|_]):-
%    format("back to ~w~n",[Cur]).

collect_refs([],[]).
collect_refs([H|T],Out):-
    pdt_strip_anotation(H,_,(Head,_)),
    (	memberchk(file_refs(Refs),Head),ground(Refs)
    ->  sort(Refs,SortedRefs)
    ;	SortedRefs=[]
    ),
    collect_refs(T,Rest),
    merge_set(SortedRefs,Rest,Out).
    
    