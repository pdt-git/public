/* $LICENSE_MSG$(ld) */

:-module(fileref_annotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).

:- pdt_annotator([interleaved,file],[]).


%term_pre_annotation_hook(Stack,_,InTerm,OutTerm):-
%	file_refs(Stack,InTerm,OutTerm).
%
%file_pre_annotation_hook(_,_,Terms,InAnos,[references_files(Refs)|InAnos]):-
%    collect_refs(Terms,Refs).

interleaved_annotation_hook(Stack,_,InTerm,OutTerm):-
	file_refs(Stack,InTerm,OutTerm).

file_annotation_hook([File|_],_,InAnos,[references_files(Refs)|InAnos]):-
    collect_refs(File,Refs).


file_refs(Stack,InTerm,OutTerm):-
    pdt_aterm_strip_annotation(InTerm,:-Term,(Head,Tail)),
    (	nonvar(Term)
    ->	find_file_refs(Term,Refs)
    ;	Refs=[]
    ),
    resolve_file_Refs(Refs,ResRefs),
    annotate_refered_files(Stack,ResRefs),
    pdt_aterm_splice_annotation(:-Term,([file_refs(ResRefs)|Head],Tail),OutTerm).

    
find_file_refs([H,T],[H,T]).
find_file_refs(load_files([H,T],_),[H,T]).
find_file_refs(load_files(H,_),[H]):-
    \+ is_list(H).
find_file_refs(consult([H,T]),[H,T]).
find_file_refs(consult(H),[H]):-
    \+ is_list(H).
find_file_refs(ensure_loaded([H,T]),[H,T]).
find_file_refs(ensure_loaded(H),[H]):-
    \+ is_list(H).
find_file_refs(include(H),[H]):-
    \+ is_list(H).    
find_file_refs(use_module([H,T]),[H,T]).
find_file_refs(use_module(H),[H]):-
    \+ is_list(H).
find_file_refs(use_module([H,T],_),[H,T]).
find_file_refs(use_module(H),_,[H]):-
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
	pdt_ensure_annotated(Stack).    	

%the_debug([Cur|_],H):-
%    format("~w refers to ~w~n",[Cur,H]).
%
%this_debug([Cur|_]):-
%    format("back to ~w~n",[Cur]).


collect_refs(File,SortedRefs):-
    pdt_file_record_key(term,File,Key),
    findall(RefList,
    	(	pdt_file_record(Key,ATerm),
    		pdt_aterm_term_annotation(ATerm,_,Annos),
    		pdt_member(file_refs(RefList),Annos)
    	), RefLists
    ),
    flatten(RefLists,Refs),
    sort(Refs,SortedRefs).


