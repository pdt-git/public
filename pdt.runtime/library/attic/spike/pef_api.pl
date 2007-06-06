:- module(pef_api,[
	module_name/2,
	module_owner/2,
	module_owner_nondet/2,	
	module_base/2,
	predicate_file/2,
	predicate_owner/2,
	num_clauses/2,
	resolve_predicate/4,
	resolve_module/3,
	toplevel_term/2,
	file_key/2,
	predicate_listing/1
	]).

:- use_module(library('spike/pef_base')).

predicate_listing(PredID):-
    forall(
    	pef_clause_query([predicate=PredID,number=Num,toplevel_ref=TlRef]),
    	(	pef_toplevel_recorded(_,[expanded=Term],TlRef),
    		format("% clause ~w~n",[Num]),
    		portray_clause(Term)
    	)
    ).


    

resolve_module(PID,Name,MID):-
    pef_program_module_query([program=PID,name=Name,module=MID]).

%% resolve_predicate(+MID,+Name,+Arity,-PredID).
% resolve a predicate signature within the context of a program module.
resolve_predicate(MID,Name,Arity,PredID):-% look for local predicate
	pef_predicate_query([module=MID,name=Name,arity=Arity,id=PredID]),
	!.
resolve_predicate(MID,Name,Arity,PredID):-% look for imported predicate
	pef_imported_predicate_query([module=MID,name=Name,arity=Arity,predicate=PredID]),
	!.

resolve_predicate(MID,Name,Arity,PredID):- % module extension: fall back to base
	pef_module_extension_query([id=MID,base=BaseID]),
	resolve_predicate(BaseID,Name,Arity,PredID).

num_clauses(PredID,N):-
    pef_property_query([id=PredID,key=number_of_clauses,value=N]),
    !.
num_clauses(_PredId,0).

toplevel_term(TlRef,Term):-
    pef_toplevel_recorded(_,[expanded=Term],TlRef).

predicate_file(PredId,none):-
    pef_property_query([id=PredId,key=(multifile),value=true]),
	!.
predicate_file(PredId,FileRef):-
	pef_clause_query([predicate=PredId, toplevel_ref=TlRef]),
	!,
	pef_toplevel_recorded(_,[file_ref=FileRef],TlRef).
predicate_file(_PredId,none).


predicate_owner(PredID,PID):-
    pef_predicate_query([id=PredID,module=MID]),
    module_owner(MID,PID).

module_name(MID,Name):-
    pef_module_definition_query([id=MID, name=Name]),
    !.
module_name(MID,Name):-
    pef_ad_hoc_module_query([id=MID, name=Name]),
    !.
module_name(MID,Name):-
    pef_module_extension_query([id=MID, base=Base]),
    !,
    module_name(Base,Name).
module_base(MID,Base):-
    pef_module_extension_query([id=MID, base=Base]),
    !.
module_base(MID,MID).    

module_owner(MID,PID):-	
	% currently the file that is the entry point of a program and the program
	% itself are identical
	pef_module_definition_query([id=MID,file_ref=PID]),
	!.
module_owner(MID,PID):-	
	pef_module_extension_query([id=MID,program=PID]),
	!.
module_owner(MID,PID):-	
	pef_ad_hoc_module_query([id=MID,program=PID]).

module_owner_nondet(MID,PID):-	
	pef_module_definition_query([id=MID,file_ref=PID]).
module_owner_nondet(MID,PID):-	
	pef_module_extension_query([id=MID,program=PID]).
module_owner_nondet(MID,PID):-	
	pef_ad_hoc_module_query([id=MID,program=PID]).
	
file_key(FileRef,Key):-
	pef_file_query([file_ref=FileRef,toplevel_key=Key]).	
