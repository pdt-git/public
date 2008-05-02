:- module(persistence,[]).
:- use_module(builder).
:- use_module(library('pef/pef_base')).

% BEGIN interfacing with the builder:

target_available(T):-
    pdt_builder:'$target_state'(T,state(_, available, _,_,_)).

target_depends(A,B):-
	pdt_builder:target_depends(A,B).

target_name(T,Name):-
	pdt_builder:target_key(Name,T).    
% END interfacing with the builder:

available_targets(Ts):-
	findall(T,target_available(T),Ts).    

write_target_names([],_).
write_target_names([T|Ts],Stream):-
	target_name(T,Name),    
    format(Stream,"~q.~n",[name(T,Name)]),
    write_target_names(Ts,Stream).

write_dependencies([],_).
write_dependencies([T|Ts],Stream):-
    forall(
    	target_depends(Depending,T),    	
    	format(Stream,"~q.~n",[depends(Depending,T)])
    ),
    write_dependencies(Ts,Stream).
    
write_targets([],_).
write_targets([T|Ts],Stream):-
    write_target(Stream,T),
    write_targets(Ts,Stream).

write_target(Stream,T):-
    pef_record_key(T,Key),
    forall(
    	recorded(Key,Data),
    	format(Stream,"~q.~n",[Data])
    ).
    
write_model:-
    available_targets(Ts),
    tmp_file(names,NamesFile),
    tmp_file(deps,DepsFile),
    tmp_file(data,DataFile),
    open(NamesFile,write,NamesStream),
    call_cleanup(write_target_names(Ts,NamesStream),close(NamesStream)),
    open(DepsFile,write,DepsStream),
    call_cleanup(write_dependencies(Ts,DepsStream),close(DepsStream)),
    open(DataFile,write,DataStream),
    call_cleanup(write_targets(Ts,DataStream),close(DataStream)).
    
    
	    