:-module(fileref_annotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).



term_pre_annotation_hook(Stack,_,InTerm,OutTerm):-
	file_refs(Stack,InTerm,OutTerm).

file_pre_annotation_hook(_,_,Terms,InAnos,[references_files(Refs)|InAnos]):-
    collect_refs(Terms,Refs).


file_refs(Stack,InTerm,OutTerm):-
    pdt_strip_annotation(InTerm,Term,(Head,Tail)),
    find_file_refs(Term,Refs),   
    resolve_file_Refs(Refs,ResRefs),
    annotate_refered_files(Stack,ResRefs),
    pdt_splice_annotation(Term,([file_refs(ResRefs)|Head],Tail),OutTerm).

    
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

annotate_refered_files(_,[]).
annotate_refered_files(Stack,[H|T]):-
%    the_debug(Stack,H),
    annotate_refered_file([H|Stack]),
%    this_debug(Stack),
    annotate_refered_files(Stack,T).

annotate_refered_file([H|Stack]):-
	member(H,Stack),!.    
annotate_refered_file(Stack):-
	ensure_annotated(Stack).    	

%the_debug([Cur|_],H):-
%    format("~w refers to ~w~n",[Cur,H]).
%
%this_debug([Cur|_]):-
%    format("back to ~w~n",[Cur]).

collect_refs([],[]).
collect_refs([H|T],Out):-
    pdt_strip_annotation(H,_,(Head,_)),
    (	pdt_member(file_refs(Refs),Head)
    ->  sort(Refs,SortedRefs)
    ;	SortedRefs=[]
    ),
    collect_refs(T,Rest),
    merge_set(SortedRefs,Rest,Out).
    
    