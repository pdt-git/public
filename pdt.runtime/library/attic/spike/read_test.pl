:- use_module(library('prolog_source')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).


my_read(Spec):-
    pdt_file_spec(Spec,F),
    do_read(F).

do_read(F):-
    prolog_open_source(F,In),
    repeat,
    	prolog_read_source_term(In,Term,Expanded,[]),
    	preprocess(Expanded),
    	Term==end_of_file,
    !,
    prolog_close_source(In).
    


preprocess((:-Term)):-
    find_file_refs(Term,Files),
    !,
    forall(member(F,Files),my_read(F)).
preprocess(_Term).


find_file_refs([H|T],[H|T]).
find_file_refs(load_files([H|T],_),[H|T]).
find_file_refs(load_files(H,_),[H]):-
    \+ is_list(H).
find_file_refs(consult([H|T]),[H|T]).
find_file_refs(consult(H),[H]):-
    \+ is_list(H).
find_file_refs(ensure_loaded([H|T]),[H|T]).
find_file_refs(ensure_loaded(H),[H]):-
    \+ is_list(H).
find_file_refs(include(H),[H]):-
    \+ is_list(H).    
find_file_refs(use_module([H|T]),[H|T]).
find_file_refs(use_module(H),[H]):-
    \+ is_list(H).
find_file_refs(use_module([H|T],_),[H|T]).
find_file_refs(use_module(H),_,[H]):-
    \+ is_list(H).
    