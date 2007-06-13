
/*
 some short cuts and utilities for dealing with the pef base.
 Not that much of an api really. Ment for internal use only.
*/
:- module(pef_api,[
	module_name/2,
	module_owner/2,
	module_owner_nondet/2,	
	module_base/2,
	predicate_file/2,
	predicate_owner/2,
	num_clauses/2,
	resolve_predicate/5,
	resolve_module/3,
	toplevel_term/2,
	file_key/2,
	predicate_listing/1,
	module_file/2,
	module_predicate/2,
	import_list/2,
	module_exports/2
	]).

:- use_module(library('spike/pef_base')).

%%
% predicate_listing(+PredID)
% print all clauses of a predicate to current output.
predicate_listing(PredID):-
    forall(
    	pef_clause_query([predicate=PredID,number=Num,toplevel_ref=TlRef]),
    	(	pef_toplevel_recorded(_,[expanded=Term],TlRef),
    		format("% clause ~w~n",[Num]),
    		portray_clause(Term)
    	)
    ).


    
%%
% resolve_module(+PID,+Name,-MID)
% Resolve a module name in the context of a given program.
resolve_module(PID,Name,MID):-
    pef_program_module_query([program=PID,name=Name,module=MID]).

%%
% resolve_predicate(+PID,+MID,+Name,+Arity,-PredID)
% Resolve a predicate signature in the context of a given program and context module.
resolve_predicate(PID,MID,Name,Arity,PredID):-
	import_list(MID,Imports),    
	resolve_predicate_X(PID,MID,Imports,Name,Arity,PredID).
%% resolve_predicate(+MID,+Name,+Arity,-PredID).
% resolve a predicate signature within the context of a program module.
resolve_predicate_X(_PID,MID,_,Name,Arity,PredID):-% look for local predicate
	pef_predicate_query([module=MID,name=Name,arity=Arity,id=PredID]),
	!.
resolve_predicate_X(_PID,MID,_,Name,Arity,PredID):-% look for imported predicate
	pef_imported_predicate_query([module=MID,name=Name,arity=Arity,predicate=PredID]),
	!.
resolve_predicate_X(PID,MID,Imports,Name,Arity,PredID):- % module extension: fall back to base
	pef_module_extension_query([id=MID,base=BaseID]),
	resolve_predicate_X(PID,BaseID,Imports,Name,Arity,PredID),
	!.
resolve_predicate_X(PID,_,Imports,Name,Arity,PredID):- % fall back imported modules.
    member(MName,Imports),
    resolve_module(PID,MName,MID),
    resolve_predicate(PID,MID,Name,Arity,PredID),
    !.%cuts away CPs of member/2. Don't remove.


%%
% import_list(+MID,-Imports)
% find the import list for a given module.
% @param Imports is unified with a list of module names.
import_list(MID,Imports):-
    pef_import_list_query([module=MID,list=Imports]),
    !.
import_list(MID,Imports):-
    pef_module_extension_query([id=MID,base=Base]),
    pef_import_list_query([module=Base,list=Imports]),
    !.
import_list(MID,[system]):-
	module_name(MID,user),
	!.
import_list(_MID,[user]).
    
%%
% num_clauses(+PredID,-N)
% succeeds if the predicate has N clauses.
num_clauses(PredID,N):-
    pef_property_query([id=PredID,key=number_of_clauses,value=N]),
    !.
num_clauses(_PredId,0).
%%
% toplevel_term(+TlRef,-Term)
% succeeds if Term is the expanded version of the source term recorded as toplevel TlRef.
toplevel_term(TlRef,Term):-
    pef_toplevel_recorded(_,[expanded=Term],TlRef).

%%
% predicate_file(+PredId,-FileRef)
% find out to which file a predicate belongs.
% @param FileRef is unified with the file reference number or [] if no clauses exist or if the predicate is
% multifile.
predicate_file(PredId,[]):-
    pef_predicate_property_definition_query([predicate=PredId,property=(multifile)]),
	!.
predicate_file(PredId,FileRef):-
	pef_clause_query([predicate=PredId, toplevel_ref=TlRef]),
	!,
	pef_toplevel_recorded(_,[file_ref=FileRef],TlRef).
predicate_file(_PredId,[]).

%%
% predicate_owner(+PredID,-PID)
% find the program that owns the predicate.
predicate_owner(PredID,PID):-
    pef_predicate_query([id=PredID,module=MID]),
    module_owner(MID,PID).

%% module_name(+MID,-Name)
% true if Name is the module name of MID.
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

%% 
% module_base(+MID,-Base)
% unify Base with the base module of MID if MID is a module extension, or MID otherwise.
module_base(MID,Base):-
    pef_module_extension_query([id=MID, base=Base]),
    !.
module_base(MID,MID).    

%% module_owner(+MID,-PID)
% find the program that owns a module.
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

%% module_owner(+MID,-PID)
% nondeterministic version of module_owner/2.
% can be used to find all modules owned by a program.
module_owner_nondet(MID,PID):-	
	pef_module_definition_query([id=MID,file_ref=PID]).
module_owner_nondet(MID,PID):-	
	pef_module_extension_query([id=MID,program=PID]).
module_owner_nondet(MID,PID):-	
	pef_ad_hoc_module_query([id=MID,program=PID]).

%% 
% file_key(+FileRef,-Key)	
% unify Key with the record key that is used to store toplevel records for a given file.
file_key(FileRef,Key):-
	pef_file_query([file_ref=FileRef,toplevel_key=Key]).	

%%
% module_file(+MID,-FileRef)
% find the file that defines the given module.
% @param FileRef is unified with the file reference number or with [] if the module is ad-hoc defined.
module_file(MID,FileRef):-
    module_base(MID,Base),
    pef_module_definition_query([id=Base,file_ref=FileRef]),
    !.
module_file(_MID,[]).

%%
% module_exports(+MID,-Signatures).
module_exports(MID,Signature):-
	pef_exports_query([module=MID,signature=Signature]).
module_exports(MID,Signature):-
	pef_module_extension_query([id=MID,base=Base]),
	pef_exports_query([module=Base,signature=Signature]).		
%%
% module_predicate(+DefMID,-PredID)
% true if module DefMID defines (not imports!) predicate PredID .
% Can be used to list all predicates defined by a module.
module_predicate(DefMID,PredID):-
    pef_predicate_query([id=PredID, module=DefMID]).
module_predicate(DefMID,PredID):-
    pef_module_extension_query([id=DefMID,base=Base]),
    pef_predicate_query([id=PredID, module=Base,name=Name,arity=Arity]),
    \+pef_predicate_query([module=DefMID, name=Name, arity=Arity]).