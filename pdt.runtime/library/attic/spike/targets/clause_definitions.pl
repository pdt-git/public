:- module(program_interpreter,[]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).



:- pdt_define_context(cx(program,module,file_ref,toplevel_ref)).

interprete_program(FileRef):-
    create_program(FileRef,Cx),
    pef_file_query([file_ref=Ref,toplevel_key=Key]),
    forall(
    	pef_toplevel_recorded(Key,[file_ref=FileRef,expanded=Expanded],TlRef),
    	(	cx_toplevel_ref(TlRef),
    		interprete_toplevel(Expanded,Cx)
    	)
    ).
    
interprete_toplevel( (:-(MName:Term)) , Cx):-
    get_or_create_module(MName,MID,Cx),
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel((:-Term),Cx1).    
interprete_toplevel( (MName:Term) , Cx):-
    !,
    get_module(MName,MID,Cx),
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel(Term,Cx1).    
interprete_toplevel( ((MName:Head):-Body) , CX):-
    !,
    get_or_create_module(MName,MID,Cx),    
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel((Head:-Body),Cx1).    
interprete_toplevel( (Head:-Body) , CX):-
    !,
    functor(Head,Name,Arity),
    get_or_create_predicate(Name,Arity,CX,PredID),
    add_clause(PredID,CX).
interprete_toplevel( (:-module(_,_)) , Cx):-
	!.%already dealt with.
interprete_toplevel( (:-dynamic Signatures) , Cx):-
    !,
    interprete_property_definition(Signatures,(dynamic),Cx).
interprete_toplevel( (:-module_transparent Signatures) , Cx):-
    !,
    interprete_property_definition(Signatures,(module_transparent),Cx).
interprete_toplevel( (:-multifile Signatures) , Cx):-
    !,
    interprete_property_definition(Signatures,(multifile),Cx).
interprete_toplevel( (:-thread_local Signatures) , Cx):-
    !,
    interprete_property_definition(Signatures,(thread_local),Cx).
interprete_toplevel( (:-_) , Cx):-    
    cx_toplevel_ref(Cx,TlRef),
    %pef_file_dependency_query([toplevel_ref=TlRef]),
    %!,
    forall(
    	pef_file_dependency_query([toplevel_ref=TlRef,dep_ref=DepRef]),
    	interprete_file_dependency(DepRef,Cx)
    ).
%TODO: handle modifications of the module import lists.
%TODO: todo markers :-).
    

interprete_file_dependency(Ref,Cx):-
    pdt_file_ref(File,Ref),
	catch(
		do_inclusion(File,Ref),
		cycle(names(File)),
		format("dependency cycle: ~w~n",[names(File)])
	).
    
do_inclusion(File,Ref,Cx):-	
	pdt_with_targets([names(File)],
		(	(	pef_module_definition_query([file_ref=Ref],Module)
			->  process_module_inclusion(Module,Cx)
			;	process_file_inclusion(Ref,Cx)
			)
		)		
	).

%the most acurate thing I could figure out:
    % - check if the program already loaded this file
    % - if yes, 
    % 	+ remove all non-multifile predicates defined in this file 
    %	+ remove all clauses this file adds to multifile predicates, but leave the predicate defined
    % - load the file into the current context.
    
process_file_inclusion(Ref,Cx):-
    unload_file(Ref,Cx),
    cx_set(Cx,[file_ref=Ref,toplevel_ref=_],Cx1),
    pef_file_query([file_ref=Ref,toplevel_key=Key]),
    forall(
    	pef_toplevel_recorded(Key,[file_ref=FileRef,expanded=Expanded],TlRef),
    	(	cx_toplevel_ref(TlRef),
    		interprete_toplevel(Expanded,Cx1)
    	)
    ).
    
    
process_module_inclusion(Module,Cx):-
	pef_module_definition_get(Module,[toplevel_ref=DefTlRef,name=Name,id=MID]),
    check_for_module_name_conflict(Name,MID,Cx,Outcome),
    (	Outcome=conflict(OldMID)
    ->  cx_toplevel_ref(Cx,TlRef),
    	format("Module name conflict at toplevel ~w:n existing module: ~w, new module ~w~nI will stick with the old definition.", [TlRef,OldMID,MID])
	;	Outcome=ok    
    ->	rebind_module_name(Name,MID,Cx),
	    pef_toplevel_recorded(_,[expanded=(:-module(_,Exports))],DefTlRef),
	    module_owner(MID,PID),
	    import_module_bindings(PID,Cx),
	    import_predicate_bindings(Exports,MID,Cx)
	;	true
	).


import_module_bindings(PID,Cx):-
    cx_toplevel_ref(Cx,TlRef),
    forall(
    	pef_program_module_query([program=PID,name=Name,module=Module]),
    	(	check_for_module_name_conflict(Name,Id,Cx,Outcome),
    		(	Outcome=conflict(OldID)
    		->	format("Module name conflict at toplevel ~w:n existing module: ~w, new module ~w~nI will stick with the old definition.", [TlRef,OldMID,Module])
    		;	Outcome=ok
    		->	rebind_module_name(Name,Module,Cx)
    		; 	true
    		)
    	)
    ).

import_predicate_bindings([],_Name,_Cx).
import_predicate_bindings([Export|Exports],Name,Cx):-
    import_predicate_binding(Export,Name,Cx),
    import_predicate_bindings(Exports,Name,Cx).

import_predicate_binding(op(_,_,_),_MID,_Cx). %the parser deals with this.
import_predicate_binding(Name/Arity,MID,Cx):-
    module_owner(MID,PID),
    cx_set(Cx,[module=MID,program=PID],Cx1),
    get_predicate(Name,Arity,Cx1,PredId),
    (	get_predicate(Name,Arity,Cx,OldPredId)
    ->	format("Predicate name conflict at toplevel ~w:~n existing predicate: ~w, new predicate ~w~n I will stick with the existing definition.~n",[TlRef,OldPredId,PredId])
    ;	cx_module(Cx,ContextModuleID),
    	pef_imported_predicate_retractall([module=ContextModuleID,name=Name,arity=Arity]),
    	pef_imported_predicate_assert([module=ContextModuleID,name=Name,arity=Arity,predicate=PredId])
    ). %TODO: multifile predicates
    
check_for_module_name_conflict(Name,Id,Cx,Outcome):-
    get_module(Name,MID,Cx),
    !,
    (	MID \== Id
	->	Outcome=conflict
	;	Outcome=redundant
	). %TODO:  merging in ad-hoc modules
check_for_module_name_conflict(_Module,_Cx,ok).    


unload_file(Ref,Cx):-
    % find all non-multifile predicates defined in the file
    cx_program(Cx,PID),
    pef_file_query([file_ref=Ref,toplevel_key=Key]),
    setof(Pred,    	    
    	(	pef_toplevel_recorded(Key,[],TlRef),
    		pef_clause_query([toplevel_ref=TlRef,predicate=Pred,id=CID]),    		
    		pef_predicate_query([id=Pred,module=MID]),
    		pef_program_module([program=PID,module=MID]),
    		pef_clause_retractall([id=CID]),
    		\+ pef_property_query([id=Pred,key=(multifile),value=true])
    	),
    	Preds
    ),
    forall(member(Pred,Preds), pef_predicate_retractall([id=Pred])).

%% get_module(+Name, -MID, +CX)
% retrieve the ID of the module currently bound to Name within the context CX.
get_module(Name,MID,Cx):-
    cx_program(Cx,PID),
	pef_program_module_query([program=PID,name=Name,module=MID]).
	
%% module(?MID, ?PID)
% succeeds if the file defining the module with id MID is also the
% entry point of the program with id PID.


/*
while a program may consist of several modules, and while a module may be 
reused in several programs, each module "belongs" to exactly one program:
- a module definition belongs to the program that has the file defining the module 
  as entry point.
- a module extension belongs to the program that "makes" the extension.
- an ad-hoc module belongs to the program that requests it.

The way things currently work, we only reuse modules across programms if they are owned by
a program whos entry point is itself a file containing a module definition.
*/
module_owner(MID,PID):-	
	% currently the file that is the entry point of a program and the program
	% itself are identical
	pef_module_definition_query([id=MID,file_ref=PID]).
module_owner(MID,PID):-	
	pef_module_extension_query([id=MID,program=PID]).
module_owner(MID,PID):-	
	pef_ad_hoc_module_query([id=MID,program=PID]).


create_program(FileRef,Cx):-
	cx_new(Cx),
	(	pef_module_definition_query([file_ref=FileRef,id=MID,name=Name])
	->	rebind_module_name(Name,MID,Cx)
	;	get_or_create_module(user,MID,Cx)
	),
	cx_get(Cx,[program=FileRef,file_ref=FileRef,module=MID]).
	
get_or_create_module(Name,MID,Cx):-
    cx_program(Cx,PID),
    (	pef_program_module_query([program=PID,name=Name,module=MID])
    ->	true
    ;	pef_reserve_id(pef_ad_hoc_module,MID),
    	pef_ad_hoc_module_assert([id=MID,name=Name,program=PID]),
    	pef_program_module_assert([program=PID,name=Name,module=MID])
    ).
    
get_or_create_predicate(Name,Arity,CX,PredID):-
	get_predicate(Name,Arity,Cx,PredID),
	!.    
get_or_create_predicate(Name,Arity,CX,PredID):-
    cx_module(Cx,MID),
    pef_reserve_id(pef_predicate,PredID),
    pef_predicate_assert([module=MID,name=Name,arity=Arity]).

rebind_module_name(Name,MID,Cx):-
    cx_program(Cx,PID),
    pef_program_module_retractall([program=PID,name=Name]),
    pef_program_module_assert([program=PID,name=Name,module=MID]).
    
get_predicate(Name,Arity,Cx,PredID):-% look for local predicate
	cx_module(Cx,MID),
	pef_predicate_query([module=MID,name=Name,arity=Arity,id=PredID]),
	!.
get_predicate(Name,Arity,Cx,PredID):-% look for imported predicate
	cx_module(Cx,MID),
	pef_imported_predicate_query([module=MID,name=Name,arity=Arity,predicate=PredID]),
	!.

get_predicate(Name,Arity,Cx,PredID):- % module extension: fall back to base
	cx_module(Cx,MID),
	pef_module_extension_query([id=MID,base=BaseID]),
	cx_set_module(Cx,MID,Cx1),
	get_predicate(Name,Arity,Cx1,PredID).
	
add_clause(PredID,CX):-
    % if the predicate is defined in a module and if this module is owned by a program other than the current,
    % we may not modify it.
    % Instead, we rebind the module name to a new module extension with the defining module
    % set as base. We then copy the predicate with all existing clauses to the extension and add the clause there.
    pef_predicate_query([id=PredId,name=Name, arity=Arity,module=MID]),
    cx_program(Cx,Program),
    (	module_owner(MID,Program)
    ->  % all green, we own the module (and therefor the predicate)
    	do_add_clause(PredID,Cx)
    ;	% not ours. We must not modify it, but have to create a new module and work on a copy of the predicate
    	% if the module is a real module definition, extend it
    	pef_module_definition_query([id=MID])
    ->	extend_module(MID,Cx,NewMID),
    	copy_predicate_to_module(PredID,NewMID,Cx,NewPredID),		
    	do_add_clause(NewPredID,Cx)
	;	% if the module is ad-hoc defined,or a module extension, clone it.
		clone_module(MID,Cx,NewMID),
		pef_predicate_query([id=NewPredID,module=NewMID,name=Name,arity=Arity]),
		do_add_clause(NewPredID,Cx)
    ).
		
    