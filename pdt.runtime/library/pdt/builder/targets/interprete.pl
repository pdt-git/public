:- module(interprete,[]).
 
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/parse')).



:- pdt_define_context(cx(program,module,file,toplevel,file_stack)).

pdt_builder:target_file(interprete(F),F).

pdt_builder:build_hook(interprete(AbsFile)):-
    interprete:my_build_hook(AbsFile).

my_build_hook(AbsFile):-    
	%pdt_forget_program(AbsFile),
	pdt_interprete_program(AbsFile).

pdt_builder:invalidate_hook(parse(AbsFile)):-
    pdt_invalidate_target(interprete(AbsFile)).
pdt_builder:invalidate_hook(interprete(DepFile)):-
    get_pef_file(DepFile,DepRef),
    pef_file_dependency_query([dependency=DepRef,depending=FileRef]),
    get_pef_file(File,FileRef),
    pdt_invalidate_target(interprete(File)).
/*
pdt_forget_program(AbsFile):-
    pdt_invalidate_target(interprete(AbsFile)),
    get_pef_file(AbsFile,Ref),
    (	pef_program_query([file=Ref,id=PID])
    ->	forget_program(PID)
    ;	true
    ).
*/
/*pdt_forget_program(AbsFile):-
    get_pef_file(AbsFile,Ref),
    pef_program_cleanupall([file=Ref]).
    
%% forget_program(+PID)
% Anihilate program PID.
% forget all modules owned by the program PID.
% Also forget all module name bindings within the program.
% Finally, forget all problems associated with the program. (NOT IMPLEMENTED YET).
%
% Does NOT check for dangling references to the program or its components.
forget_program(PID):-
	 pef_program_module_retractall([program=PID]),	 
	 forall(module_owner_nondet(MID,PID),delete_module(MID)).
*/
%% delete_module(+MID)
% Rid the world of module MID.
% delete all predicates defined by the module.%
% delete all bindings imported from other modules.
% delete the module itself, if it is an extension, or an ad-hoc module. Don't delete module definitions or exports,
% they are created by the parser, not by the interpreter.
%
% Does NOT check for dangling references to the module or its contents. 
delete_module(MID):-
    forall(pef_predicate_query([id=PredID,module=MID]), delete_predicate(PredID)),
%    pef_exports_retractall([module=MID]),
    pef_imported_predicate_retractall([module=MID]),
%    pef_module_definition_retractall([id=MID]),
    pef_module_extension_retractall([id=MID]),
    pef_ad_hoc_module_retractall([id=MID]),
    pef_property_retractall([pef=MID]).

%% delete_predicate(+PredID)
% extinguish predicate PredID.
% Remove all clauses and properties. 
% Remove the predicate itself.
% Does NOT check for dangling references to the predicate. 
delete_predicate(PredID):-
    pef_clause_retractall([predicate=PredID]),
    pef_property_retractall([pef=PredID]),
    pef_predicate_property_definition_retractall([predicate=PredID]),
    pef_predicate_retractall([id=PredID]).

%% clear_predicate(PredID,Cx).
% like delete, but leaves the predicate defined.
clear_predicate(PredID):-
    pef_clause_retractall([predicate=PredID]),
    pef_predicate_property_definition_retractall([predicate=PredID]),    
    pef_property_retractall([pef=PredID]).
    

pdt_interprete_program(Abs):-
    get_pef_file(Abs,FileRef),
	pdt_request_target(parse(Abs)),
	interprete_program(FileRef).

interprete_program(FileRef):-    
    create_program(FileRef,Cx),

    forall(
    	pef_toplevel_query([file=FileRef,id=TlRef,expanded=Expanded]),
    	(	cx_toplevel(Cx,TlRef),
    		(	interprete_toplevelXXX(Expanded,Cx)
    		->	true
    		;	throw(failed(interprete_toplevelXXX(Expanded,Cx)))
    		)
    	)
    ).


interprete_toplevelXXX(Expanded,Cx):-
    (	\+ \+ Expanded = (pdt_builder:build_hook(parse(_)):-_)
    -> 	spyme
    ;	true
    ),
	interprete_toplevel(Expanded,Cx).    

interprete_toplevel(end_of_file,_):-
	!.    
interprete_toplevel( (:-(MName:Term)) , Cx):-
    !,
    get_or_create_module(MName,MID,Cx),
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel((:-Term),Cx1).    
interprete_toplevel( (MName:Term) , Cx):-
    !,
    get_or_create_module(MName,MID,Cx),
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel(Term,Cx1).    
interprete_toplevel( ((MName:Head):-Body) , Cx):-
    !,
    get_or_create_module(MName,MID,Cx),    
    cx_set_module(Cx,MID,Cx1),
    interprete_toplevel((Head:-Body),Cx1).    
interprete_toplevel( (Head:-_Body) , Cx):-
    !,
    functor(Head,Name,Arity),    
    create_my_own_predicate_version(Name,Arity,MyPredID,Cx,Cx1),
   do_add_clause(MyPredID,Cx1).
    
interprete_toplevel( (:-module(_,_)) , _Cx):-
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
interprete_toplevel( (:-Body) , Cx):-        
    !,
	cx_toplevel(Cx,TlRef),    
    functor(Body,Name,_Arity),
    
    forall(
    	pef_file_dependency_query([toplevel=TlRef,dependency=DepRef]),
    	interprete_file_dependency(Name,DepRef,Cx)
    ).
interprete_toplevel( Head , Cx):-
    !,
    functor(Head,Name,Arity),
    create_my_own_predicate_version(Name,Arity,MyPredID,Cx,Cx1),
   do_add_clause(MyPredID,Cx1).
interprete_toplevel(T,Cx):-
     throw(interprete_toplevel_failed(T,Cx)).
%TODO: handle modifications of the module import lists.
%TODO: todo markers :-).
    
    
interprete_property_definition((Head,Tail),Property,Cx):-
	interprete_property_definition(Head,Property,Cx),
	interprete_property_definition(Tail,Property,Cx).
interprete_property_definition(Module:Head,Property,Cx):-		
    get_or_create_module(Module,MID,Cx),    
    cx_set_module(Cx,MID,Cx1),
	interprete_property_definition(Head,Property,Cx1).
interprete_property_definition((Module:Name)/Arity,Property,Cx):-		
    get_or_create_module(Module,MID,Cx),    
    cx_set_module(Cx,MID,Cx1),
	interprete_property_definition(Name/Arity,Property,Cx1).
interprete_property_definition(Name/Arity,Property,Cx):-	
    create_my_own_predicate_version(Name,Arity,PredID,Cx,Cx1),
    cx_toplevel(Cx1,TlRef),
    pef_predicate_property_definition_assert([predicate=PredID,property=Property,toplevel=TlRef]).
    
interprete_file_dependency(Type,Ref,Cx):-
    get_pef_file(File,Ref),
	catch(
		do_inclusion(Type,File,Ref,Cx),
		cycle(T),
		debug(interprete(todo),"TODO: warn about dependency cycle: ~w~n",[T])
	).
    
do_inclusion(Type,File,Ref,Cx):-	
	pdt_request_target(parse(File)), 
	do_inclusion_X(Type,File,Ref,Cx).

do_inclusion_X(Type,File,Ref,Cx):-
    
    (	pef_module_definition_query([file=Ref,id=MID])
	->  catch(
			pdt_request_target(interprete(File)),
			error(cycle(T)),
			(	debug(interprete(todo),"TODO: warn about dependency cycle: ~w~nWe try to go on with incomplete information.",[T]),
				process_module_inclusion(Type,Ref,MID,Cx)
			)
		),
		process_module_inclusion(Type,Ref,MID,Cx)			
	;	process_file_inclusion(Type,Ref,Cx)
	).


process_file_inclusion(ensure_loaded,Ref,Cx):- %The file was already loaded in this context.
	cx_get(Cx,[program=PID,module=MID]),
	module_name(MID,ModName),
	pef_program_file_query([program=PID,file=Ref,module_name=ModName]),
	!.
process_file_inclusion(_,Ref,Cx):-
    cx_file_stack(Cx,Stack),
    memberchk(Ref,Stack),
    !,
	debug(interprete(todo),"TODO: warn about dependency cycle. We will not process ~w inline.~nWe try to go on with incomplete information.~n",[Ref]).
process_file_inclusion(Type,Ref,Cx):- %The file was not yet loaded in this context, or reinterpretation is explicitly requested.
    unload_file(Ref,Cx),
	cx_get(Cx,[program=PID,module=MID,file_stack=Stack]),
	module_name(MID,ModName),
    cx_set(Cx,[file=Ref,toplevel=_,file_stack=[Ref|Stack]],Cx1),
    forall(
    	pef_toplevel_query([id=TlRef,file=Ref,expanded=Expanded]),
    	(	cx_toplevel(Cx1,TlRef),
    		interprete_toplevel(Expanded,Cx1)
    	)
    ),
	(	Type==ensure_loaded
    ->	pef_program_file_assert([program=PID,module_name=ModName,file=Ref, force_reload=false])
    ;	pef_program_file_assert([program=PID,module_name=ModName,file=Ref, force_reload=true])    
    ).
    
process_module_inclusion(use_module,Ref,MID,Cx):-    
   	cx_program(Cx,PID),
	module_name(MID,ModName),
	pef_program_file_query([program=PID,file=Ref,module_name=ModName]),
	!,
	(	resolve_module(PID,ModName,LocalMID)
    ->	import_public_predicates(LocalMID,Cx)	
    ;	spyme,throw(failed(resolve_module(PID,ModName,LocalMID)))
    ).
process_module_inclusion(Type,_Ref,MID,Cx):-
    module_owner(MID,OtherPID),
    unload_obsolete_files(OtherPID,Cx),
    merge_program(OtherPID,Cx),
	(	(	Type==ensure_loaded 
		; 	Type==use_module
		)
    ->	merge_files(false,OtherPID,Cx)
    ;	merge_files(true,OtherPID,Cx)
    ),
    % we have to resolve the module name in the current program
    % it may have been extended
    module_name(MID,MName),
    cx_program(Cx,PID),
    (	resolve_module(PID,MName,LocalMID)
    ->	import_public_predicates(LocalMID,Cx)	
    ;	spyme,throw(failed(resolve_module(PID,MName,LocalMID)))
    ).



%% merge_files(+LocalForce,+PID,+Cx).
% merge program <--> file bindings of PID into the current program.
% LocalForce is either true or false, indicating whether PID is merged through a 
% directive like consult,i.e. one that forces reloading itself.  
merge_files(LocalForce,PID,Cx):-
    forall(
    	pef_program_file_query([program=PID,module_name=NewModName,file=FileRef,force_reload=Force]),
    	(	merge_file(FileRef,NewModName,Force,LocalForce,Cx)
    	->	true
    	;	throw(failed(merge_file(PID,FileRef,NewModName,Force,LocalForce,Cx)))
    	)
    ).
%%merge_force(+Current,+Local,+Merge,-New).
merge_force(true,_Local,_Merge,true):-!.
merge_force(false,true,true,true):-!.
merge_force(_,_,_,false).    
    
merge_file(FileRef,NewModName,Force,LocalForce,Cx):-
    /*
    Force is true iff the program beeing merged(PID) forces reload of the file (FileRef).
    LocalForce is true iff the directive inducing the merge of PID forces reload.
    CurrentForce is true iff the current program forces reload of the file
    NewForce should be CurrentForce || (LocalForce && Force)          
    */
	cx_program(Cx,CurrentPID),
	(	pef_program_file_query([program=CurrentPID,file=FileRef,force_reload=CurrentForce])
    ->	pef_program_file_retractall([program=CurrentPID,file=FileRef])
	;	CurrentForce=false
	),
	merge_force(CurrentForce,LocalForce,Force,NewForce),
	pef_program_file_assert([program=CurrentPID,file=FileRef, module_name=NewModName,force_reload=NewForce]).
	
/* there is somethig wrong with this clause. I do not completely understand what it was meant for

merge_file(FileRef,FileRef,NewModName,Force,LocalForce,Cx):-
	% File a module file. This predicate is only called by process_module_inclusion/4.
    cx_program(Cx,MyPID),
    merge_force(Force,LocalForce,NewForce),
    pef_program_file_retractall([program=MyPID,file=FileRef]),
    pef_program_file_assert([program=MyPID,file=FileRef,module_name=NewModName,force_reload=NewForce]).

merge_file(_PID,FileRef,NewModName,Force,_LocalForce,Cx):-
	% File a module file. This predicate is only called by process_module_inclusion/4.
    cx_program(Cx,MyPID),
	(	pef_program_file_query([program=MyPID,file=FileRef,force_reload=MyForce])
    ->	pef_program_file_retractall([program=MyPID,file=FileRef]),
	    merge_force(Force,MyForce,NewForce)
	;	NewForce=Force
	),
    pef_program_file_assert([program=MyPID,file=FileRef,module_name=NewModName,force_reload=NewForce]).

merge_force(true,true,true):- !.    
merge_force(_,_,false).
*/

%% unload_obsolete_files(NewPID,Cx).
% unload all files that are obsoleted by merging NewPID with the current program.
unload_obsolete_files(NewPID,Cx):-
    cx_program(Cx,OldPID),
    forall(
    	obsolete_file(OldPID,NewPID,FileRef),
    	unload_file(FileRef,Cx)
    ).
%% obsolete_file(+OldPID,+NewPID, -FileRef)
% succeeds if mergin NewPID into OldPID obsoletes the information collected for FileRef in OldPID. 
obsolete_file(OldPID,NewPID,FileRef):-
    pef_program_file_query([program=OldPID,module_name=OldModName,file=FileRef]),
    pef_program_file_query([program=NewPID,module_name=NewModName,file=FileRef,force_reload=Force]),    
    (	Force==true
    ->	true
    ;	OldModName \== NewModName
    ).

merge_program(PID,Cx):-
    import_module_bindings(PID, Cx).



import_module_bindings(PID,Cx):-
    forall(
    	pef_program_module_query([program=PID,name=Name,module=NewMID]),
    	(	import_module_binding(Name,NewMID,Cx)
    	->	true
    	;	throw(failed(import_module_binding(Name,NewMID,Cx)))
    	)
    ).



import_module_binding(Name,NewMID,Cx):-
    (	get_module(Name,OldMID,Cx)
	->	catch(
			merge_modules(Name,OldMID,NewMID,Cx,_MergedMID),
			module_name_conflict(OldMID,NewMID),
			%debug(interprete(todo),"TODO: create problem pef for ~w (context: ~n",[module_name_conflict(OldMID,NewMID),Cx])
			(	pef_reserve_id(pef_module_name_clash,ID),
				cx_get(Cx,[program=PID,toplevel=TlRef]),
				pef_module_name_clash_assert([id=ID,program=PID,toplevel=TlRef,first=OldMID,second=NewMID])
			)
		)
	;	rebind_module_name(Name,NewMID,Cx)
	).	

import_public_predicates(MID,Cx):-
    forall(
    	module_exports(MID,Name/Arity),
    	import_predicate_binding(Name,Arity,MID,Cx)
    ).

% establishes a "forwarding" of the symbol Name/Arity so that it is resolved
% in the context of MID. 
import_predicate_binding(Name,Arity,MID,Cx):-
    module_owner(MID,PID),
    module_name(MID,FromName),
    cx_set(Cx,[module=MID,program=PID],Cx1),
    get_predicate(Name,Arity,Cx1,PredId),
    !,
    (	get_predicate(Name,Arity,Cx,OldPredId)    
    ->	(	OldPredId==PredId
    	->	true
	    ;	cx_get(Cx,[toplevel=TlRef,program=CurrentPID]),
	    	pef_reserve_id(predicate_name_clash,ID),
	%		debug(interprete(todo),"TODO: add an error marker. Predicate name conflict at toplevel ~w:~n existing predicate: ~w, new predicate ~w~n I will stick with the existing definition.~n",[TlRef,OldPredId,PredId])
			pef_predicate_name_clash_assert([id=ID,program=CurrentPID,toplevel=TlRef,module=MID,first=OldPredId,second=PredId])
		)
    ;	cx_module(Cx,ContextModuleID),
    	pef_imported_predicate_retractall([module=ContextModuleID,name=Name,arity=Arity]),
    	pef_imported_predicate_assert([module=ContextModuleID,name=Name,arity=Arity,from_name=FromName])
    ).
import_predicate_binding(Name,Arity,MID,Cx):-
	pef_reserve_id(pef_unresolved_export,ID),
	cx_get(Cx,[program=PID,toplevel=TlRef]),
	pef_unresolved_export_assert([id=ID,program=PID,toplevel=TlRef,name=Name,arity=Arity,module=MID,export=todo]).    

    
    
    
    
    

%% get_module(+Name, -MID, +CX)
% retrieve the ID of the module currently bound to Name within the context CX.
get_module(Name,MID,Cx):-
    cx_program(Cx,PID),
	pef_program_module_query([program=PID,name=Name,module=MID]).
	



create_program(FileRef,Cx):-
    create_program_XXX(FileRef,Cx),
    !.
create_program(FileRef,Cx):-
	throw(failed(create_program(FileRef,Cx))).    
    
create_program_XXX(FileRef,Cx):-
	cx_new(Cx),
	pef_reserve_id(pef_program,PID),
	pef_program_assert([id=PID,file=FileRef]),
	cx_program(Cx,PID),
	(	pef_module_definition_query([file=FileRef,id=MID,name=Name])
	->	%begin DEBUG 
		pef_property_assert([pef=PID,key=trying,value=rebind_module_name(Name,MID,Cx)]),
		%end DEBUG
		rebind_module_name(Name,MID,Cx),
		%begin DEBUG 
		pef_property_assert([pef=PID,key=success,value=rebind_module_name(Name,MID,Cx)]),
		%end DEBUG
	
		pef_program_file_assert([program=PID,module_name=Name,file=FileRef, force_reload=false]),
		get_or_create_module(user,_,Cx), % a module user should exist in every program.
		%begin DEBUG 
		pef_property_assert([pef=PID,key=program_type,value=module])
		%end DEBUG
	;	get_or_create_module(user,MID,Cx),
		pef_program_file_assert([program=PID,module_name=user,file=FileRef, force_reload=false]),
		%begin DEBUG 
		pef_property_assert([pef=PID,key=program_type,value=non_module])
		%end DEBUG
	),
	cx_get(Cx,[file=FileRef,file_stack=[FileRef],module=MID]).
	
get_or_create_module(Name,MID,Cx):-
    cx_program(Cx,PID),
    (	pef_program_module_query([program=PID,name=Name,module=MID])
    ->	true
    ;	pef_reserve_id(pef_ad_hoc_module,MID),
    	pef_ad_hoc_module_assert([id=MID,name=Name,program=PID]),
    	debug(interprete(debug), "Created ad-hoc module ~w. ~w~n",[MID,Cx]),
    	rebind_module_name(Name,MID,Cx)
    ).
    

	

%%
% rebind_module_name(+Name,+MID,+Cx)
% binds a module to a module name.
% If another module was bound to this name before, this binding is removed. The module
% itself however will remain defined.
rebind_module_name(Name,MID,Cx):-
    cx_program(Cx,PID),
    pef_program_module_retractall([program=PID,name=Name]),
    pef_program_module_assert([program=PID,name=Name,module=MID]),
    debug(interprete(debug),"Program ~w now binds name \"~w\" to module ~w. ~w~n",[PID,Name,MID,Cx]).

unbind_module_name(Name,Cx):-
    cx_program(Cx,PID),
    pef_program_module_retractall([program=PID,name=Name]),
    debug(interprete(debug),"binding of module name \"~w\" removed from program ~w. ~w~n",[Name,PID,Cx]).


   	


set_num_clauses(PredID,N):-
    ( PredID == 38 -> debugme ; true ),
	pef_property_retractall([pef=PredID,key=number_of_clauses]),
	pef_property_assert([pef=PredID,key=number_of_clauses,value=N]).
    
do_add_clause(PredID,Cx):-
    predicate_file(PredID,PredFile),
    cx_file(Cx,CurrentFile),
    do_add_clause(PredFile,CurrentFile,PredID,Cx).
    
do_add_clause([],_,PredId,Cx):-    
    !,
    really_do_add_clause(PredId,Cx).
do_add_clause(_,[],PredId,Cx):-    
    !,
    really_do_add_clause(PredId,Cx).
do_add_clause(F,F,PredId,Cx):-    
    !,
    really_do_add_clause(PredId,Cx).
do_add_clause(_,_,PredID,Cx):-    
	debug(interprete(todo),"TODO: problem marker informing us that a \"static\" predicate ~w is redifined (context: ~w)~n",[PredID,Cx]),
	clear_predicate(PredID),
    really_do_add_clause(PredID,Cx).


really_do_add_clause(PredID,Cx):-
	cx_get(Cx,[toplevel=TlRef,file=FileRef]),
    num_clauses(PredID,N),
    M is N + 1,
    pef_clause_assert([predicate=PredID,number=M,toplevel=TlRef]),
    (	M == 1
    ->	(	pef_property_query([pef=PredID,key=file])
    	->	throw(something_wrong_with_clause_order)
    	;  	pef_property_assert([pef=PredID,key=file,value=FileRef])
    	)
    ;	true
    ),
    toplevel_term(TlRef,Term),
    debug(interprete(debug),"added toplevel ~w (~w) as clause ~w to predicate ~w. ~w~n",[TlRef,Term,M,PredID,Cx]),
    set_num_clauses(PredID,M).    
		
merge_modules(Name,OldMID,NewMID,Cx,MID):-
    debug(interprete(info),"trying to merge modules ~w and ~w. (context: ~w)~n",[OldMID,NewMID,Cx]),
    pef_type(OldMID,OldType),
    pef_type(NewMID,NewType),
    (	module_base(OldMID,Base), module_base(NewMID,Base)
    ->	SameBase=true
    ;	SameBase=false
    ),
    (	merge_modules(OldType,NewType,SameBase,Name,OldMID,NewMID,Cx,MID)
    ->	true
    ;	throw(failed(merge_modules(OldType,NewType,SameBase,Name,OldMID,NewMID,Cx,MID)))
    ).


merge_modules(Type,Type,true,_Name,MID,MID,_Cx,MID):- % 01
	!,
	debug(interprete(debug),"case 01~n",[]).
merge_modules(pef_module_definition,pef_module_definition,false,_Name,OldMID,NewMID,_Cx,_MID):- %02
	debug(interprete(debug),"case 02~n",[]),
    throw(module_name_conflict(OldMID,NewMID)).
merge_modules(pef_module_definition,pef_module_extension,true,Name,OldMID,NewMID,Cx,MID):- %03
	debug(interprete(debug),"case 03~n",[]),
	cx_program(Cx,PID),
	(	module_owner(OldMID,PID)
	->	merge_module(append,NewMID,OldMID,Cx),
		MID=OldMID
	;	rebind_module_name(Name,NewMID,Cx),
		MID=NewMID
	). 
merge_modules(pef_module_definition,pef_module_extension,false,_Name,OldMID,NewMID,_Cx,_MID):- %04
	debug(interprete(debug),"case 04~n",[]),
%	debug(interprete(todo),"TODO: error marker: loadin module ~w failed, because a module ~w with the same name is already loaded. (context: ~w)~n",[NewMID,OldMID,Cx]),
	debug(interprete(info),"Our model may be incorrect now!!!",[]),
	throw(module_name_conflict(OldMID,NewMID)).
    
merge_modules(pef_module_definition,pef_ad_hoc_module,_SameBase,Name,OldMID,NewMID,Cx,MID):- %05
	debug(interprete(debug),"case 05~n",[]),
    cx_program(Cx,PID),
    (	module_owner(OldMID,PID)
    ->	MID=OldMID
    ;	extend_module(OldMID,MID,Cx)
    ),
    merge_module(append,NewMID,MID,Cx),
    rebind_module_name(Name,MID,Cx).
    
merge_modules(pef_module_extension,pef_module_definition,true,_Name,MID,_New,_Cx,MID):-
	debug(interprete(debug),"case 06~n",[]). %06
merge_modules(pef_module_extension,pef_module_definition,false,_Name,OldMID,NewMID,_Cx,_MID):- %07
	debug(interprete(debug),"case 07~n",[]),
    throw(module_name_conflict(OldMID,NewMID)).
merge_modules(pef_module_extension,pef_module_extension,true,Name,OldMID,NewMID,Cx,NewMID):- %08
	debug(interprete(debug),"case 08~n",[]),

    cx_program(Cx,PID),
    (	module_owner(OldMID,PID)
    ->	MID=OldMID, MergeMID=NewMID, Order=append
%    ;	module_owner(NewMID,PID)
%    ->	MID=NewMID, MergeMID=OldMID, Order=prepend
    ;	copy_module(OldMID,MID,Cx),
    	MergeMID=NewMID, Order=append
    ),
    merge_module(Order,MergeMID,MID,Cx),
    rebind_module_name(Name,MID,Cx).
merge_modules(pef_module_extension,pef_module_extension,false,_Name,OldMID,NewMID,Cx,_MID):- %09
	debug(interprete(debug),"case 09",[]),
   	debug(interprete(todo),"TODO: error marker: loadin module ~w failed, because a module ~w with the same name is already loaded. (context: ~w)~n",[NewMID,OldMID,Cx]),
	debug(interprete(info),"Our model may be incorrect now!!!",[]),
	throw(module_name_conflict(OldMID,NewMID)).
merge_modules(pef_module_extension,pef_ad_hoc_module,_SameBase,Name,OldMID,NewMID,Cx,MID):- %10
	debug(interprete(debug),"case 10~n",[]),
    cx_program(Cx,PID),
    (	module_owner(OldMID,PID)
    ->	MID=OldMID
    ;	copy_module(OldMID,MID,Cx),
	    rebind_module_name(Name,MID,Cx)
    ),
    merge_module(append,NewMID,MID,Cx).
    
merge_modules(pef_ad_hoc_module,pef_module_definition,_SameBase,Name,OldMID,NewMID,Cx,MID):-%11
	debug(interprete(debug),"case 11~n",[]),
	
    cx_program(Cx,PID),
    /*(	module_owner(NewMID,PID)
    ->	MID=NewMID
    ;*/	extend_module(NewMID,MID,Cx),
	    rebind_module_name(Name,MID,Cx)
    /*)*/,
    merge_module(prepend_abolish,OldMID,MID,Cx),
    (	module_owner(OldMID,PID)
    ->	delete_module(OldMID)
    ;	true
   	).
merge_modules(pef_ad_hoc_module,pef_module_extension,_SameBase,Name,OldMID,NewMID,Cx,MID):-%12
	debug(interprete(debug),"case 12~n",[]),
    cx_program(Cx,PID),
  /*  (	module_owner(NewMID,PID)
    ->	MID=NewMID
    ;*/	copy_module(NewMID,MID,Cx)
    /*)*/,
    merge_module(prepend_abolish,OldMID,MID,Cx),
    rebind_module_name(Name,MID,Cx),
    (	module_owner(OldMID,PID)
    ->	delete_module(OldMID)
    ;	true
   	).
merge_modules(pef_ad_hoc_module,pef_ad_hoc_module,_SameBase,Name,OldMID,NewMID,Cx,NewMID):-%13
	debug(interprete(debug),"case 13~n",[]),
    cx_program(Cx,PID),
    (	module_owner(OldMID,PID)
    ->	MID=OldMID, MergeMID=NewMID,Order=append
%    ;	module_owner(NewMID,PID)
%    ->	MID=NewMID, MergeMID=OldMID, Order=prepend
    ;	copy_module(OldMID,MID,Cx),
    	MergeMID=NewMID,Order=append
    ),
    merge_module(Order,MergeMID,MID,Cx),
    rebind_module_name(Name,MID,Cx).
    

merge_module(prepend_abolish,MergeMID,MID,Cx):-
    debug(interprete(debug),"prepending module ~w before ~w (prepend_abolish). ~w~n",[MergeMID,MID,Cx]),
    %special case. only prepend predicates that are multifile or dynamic
    % this is used in cases 11 and 12: when a module definition is encountered and an ad-hoc definition
    % for this module already exists, the interpreter abolishes all predictes that are neither dynamic
    % or multifile. (note that dynamic predicates are redefined if another definition is encountered.)
    !, 
    cx_set_module(Cx,MID,Cx1),    
    forall(
    	pef_predicate_query([id=PredId,name=Name, arity=Arity,module=MergeMID]),
    	(	(	pef_predicate_property_definition_query([predicate=PredId,property=(multifile)])
    		;	pef_predicate_property_definition_query([predicate=PredId,property=(dynamic)])
    		)
    	->	% we can savely discard the changed context, since the module will not change (we own it already) 
    		create_my_own_predicate_version(Name,Arity,MergedPredId,Cx1,_),    		
	    	merge_predicates(prepend,PredId,MergedPredId,Cx/* Not a typo! Cx1 is only used for looking up the predicate */),
    		merge_properties(prepend,PredId,MergedPredId)
    	;	%debug(interprete(todo),"TODO: add problem marker \"loading module ~w abolishes predicate ~w.\" (context: ~w)~n",[MID,PredId,Cx])
    		pef_reserve_id(pef_predicate_abolished,ID),
    		cx_get(Cx,[program=PID,toplevel=TlRef]),
    		pef_predicate_abolished_assert([id=ID,program=PID,toplevel=TlRef,module=MID,predicate=PredId])
    	)    	
    ).


merge_module(Order,MergeMID,MID,Cx):-
    debug(interprete(debug),"merging module ~w into ~w (~w). ~w~n",[MergeMID,MID,Order,Cx]),
    cx_set_module(Cx,MID,Cx1),    
    forall(
    	pef_predicate_query([id=PredId,name=Name, arity=Arity,module=MergeMID]),
    	(	create_my_own_predicate_version(Name,Arity,MergedPredId,Cx1,_),	
	    	merge_predicates(Order,PredId,MergedPredId,Cx)  		
    	)    	
    ).

spyme.

%%
% merge_predicates(+Order,+SourcePred,+TargetPred)
% 
% any predicate is either multifile, or it is associated with at most one file.
% two predicates can only be merged if at least one of them is multifile or has no clauses.
% otherwise the one that is encountered first is abolished.
% This predicate does never modify SourcePred.
%
% TODO: name clashes involving imported predicates.
% conflict and target comes after source (prepend): clear target and merges source
% conflict and target comes before source (append): no change.
%
merge_predicates(Order,SourcePred,TargetPred,Cx):-
    predicate_file(SourcePred,SourceFile),
	predicate_file(TargetPred,TargetFile),    
	merge_predicates(SourceFile,TargetFile,Order,SourcePred,TargetPred,Cx).

merge_predicates([],_,Order,SourcePred,TargetPred,Cx):-
    !,
    merge_properties(Order,SourcePred,TargetPred),
    merge_clauses(Order,SourcePred,TargetPred,Cx).
merge_predicates(_,[],Order,SourcePred,TargetPred,Cx):-
    !,
    merge_properties(Order,SourcePred,TargetPred),
    merge_clauses(Order,SourcePred,TargetPred,Cx).
merge_predicates(File,File,_Order,_SourcePred,_TargetPred,_Cx):-
    !.% same file and not multifile --> the predicates are identical.
merge_predicates(_,_,prepend,SourcePred,_TargetPred,CX):-
    !,
   	debug(interprete(todo),"TODO: problem marker informing us that a \"static\" predicate ~w is redifined : (prepend, context: ~w)~n",
   			[SourcePred,CX]),
    true. %nothing to merge, target overrides source.
merge_predicates(_,_,append,SourcePred,TargetPred,CX):-    
	debug(interprete(todo),"TODO: problem marker informing us that a \"static\" predicate ~w is redifined: (append, context: ~w)~n",
   			[TargetPred,CX]),
	clear_predicate(TargetPred),
    merge_properties(append,SourcePred,TargetPred), 
    merge_clauses(append,SourcePred,TargetPred,CX).

append_clauses([],_TargetPred,N,N).
append_clauses([C|Cs],TargetPred,N,M):-
    I is N +1,
    pef_clause_get(C,[toplevel=TlRef]),
    pef_clause_assert([predicate=TargetPred,number=I,toplevel=TlRef]),
    toplevel_term(TlRef,Term),
    debug(interprete(debug),"appending toplevel ~w (~w) as clause ~w to predicate ~w. ~n",[TlRef,Term,I,TargetPred]),
    append_clauses(Cs,TargetPred,I,M).

	
merge_clauses(append,SourcePred,TargetPred,CX):-
	nb_setval(program_interpreter_clause_number,0),
	num_clauses(TargetPred,N),

	( TargetPred==38 -> debugme ; true),
    debug(interprete(debug),"appending clauses of ~w after ~w. ~w~n",[SourcePred,TargetPred,CX]),

    findall( C,
    	(	pef_clause_query([predicate=SourcePred,toplevel=TlRef],C),
    		% only add the clause if the toplevel isn't already used!
    		(	pef_clause_query([predicate=TargetPred,toplevel=TlRef,number=I])	
    		->	debug(interprete(debug),
    				"Toplevel ~w already apears as clause number ~w in predicate ~w. Will not be added. ~w~n",
    				[TlRef,I,TargetPred,CX]
    			),
    			fail
    		;	true
    		)
    	),
    	Cs
    ),
    append_clauses(Cs,TargetPred,N,M),
    set_num_clauses(TargetPred,M).
    
merge_clauses(prepend,SourcePred,TargetPred,CX):-
	nb_setval(program_interpreter_clause_number,0),
   	
    debug(interprete(debug),"prepending clauses of ~w before ~w. ~w~n",[SourcePred,TargetPred,Cx]),
   	
    findall( C,
    	(	pef_clause_query([predicate=TargetPred,toplevel=TlRef],C),
    		(	pef_clause_query([predicate=SourcePred,toplevel=TlRef,number=I])	
    		->	debug(interprete(debug),
    				"Toplevel ~w already apears as clause number ~w in predicate ~w. Will not be added. ~w~n",
    				[TlRef,I,SourcePred,CX]
    			),
    			fail
    		;	true
    		)
    	),    	
    	TargetClauses
    ),
    debug(interprete(debug),"removing all clauses from predicate ~w. ~w~n",[TargetPred,Cx]),
   	pef_clause_retractall([predicate=TargetPred]),
    findall( C,   
    	pef_clause_query([predicate=SourcePred,toplevel=TlRef],C),
 		SourceClauses
    ),
    append_clauses(SourceClauses,TargetPred,0,SourceCount),
    append_clauses(TargetClauses,TargetPred,SourceCount,TotalCount),    
	set_num_clauses(TargetPred,TotalCount).

    
    	
merge_properties(_,Source,Source):-
	!.
merge_properties(_,Source,Target):-
	forall(
		pef_predicate_property_definition_query([predicate=Source,property=Key,toplevel=TlRef]),
		(	pef_predicate_property_definition_query([predicate=Target,toplevel=TlRef])		
		->	true
		;	pef_predicate_property_definition_assert([predicate=Target,property=Key,toplevel=TlRef])
		)
	).


copy_module(OldMID,NewMID,Cx):-
    pef_type(OldMID,Type),
    copy_module(Type,OldMID,NewMID,Cx).
copy_module(pef_module_definition,OldMID,_NewMID,_Cx):-
    throw(cannot_copy_module_definition(OldMID)).
copy_module(pef_module_extension,OldMID,NewMID,Cx):-
    cx_program(Cx,PID),
    pef_reserve_id(pef_module_extension,NewMID),
    pef_module_extension_query([id=OldMID,base=Base]),
    pef_module_extension_assert([id=NewMID,base=Base,program=PID]),
    merge_module(append,OldMID,NewMID,Cx),
    debug(interprete(debug),"Program ~w created a copy ~w of module extension ~w.~w~n",[PID,NewMID,OldMID,Cx]).
copy_module(pef_ad_hoc_module,OldMID,NewMID,Cx):-
    cx_program(Cx,PID),
    pef_reserve_id(pef_ad_hoc_module,NewMID),
    pef_ad_hoc_module_query([id=OldMID,name=Name]),
    pef_ad_hoc_module_assert([id=NewMID,name=Name,program=PID]),   
    merge_module(append,OldMID,NewMID,Cx),
    debug(interprete(debug),"Program ~w created a copy ~w of ad-hoc module ~w.~w~n",[PID,NewMID,OldMID,Cx]).
	
    
extend_module(OldMID,NewMID,Cx):-
	pef_type(OldMID,Type),
	(	Type==pef_module_definition
	->	cx_program(Cx,PID),
		pef_reserve_id(pef_module_extension,NewMID),
	    pef_module_extension_assert([id=NewMID,base=OldMID,program=PID]),
	    
	    % BEGIN debug 
	    % I wonder where those empty extensions come frome...
	    cx_toplevel(Cx,TlID),
	    pef_property_assert([pef=NewMID,key=toplevel,value=TlID]),
	    %END debug
	    
	    debug(interprete(debug),"Program ~w created an extension ~w of module ~w.~w~n",[PID,NewMID,OldMID,Cx])
	;	throw(cannot_extend_virtual_module(OldMID))
	).



:-prolog_load_context(source,Me),assert(me_source(Me)).



%%
% delete_or_shift_clauses(+PredID,+FileRef,+Start,-TotalDeleted).
% Deletes clauses from a predicate, shifting remaining predicates as necessary.
% 
% @param PredID the Predicate from which the clauses shall be related.
% @param FileRef the File which contains the clauses
% @param Start number of the first clause in the file.
% @param Deleted total number of clauses deleted.
delete_or_shift_clauses(PredID,FileRef,Start,Deleted):-
	% find all clauses of pred with clause number >= start 
    findall( C,
    	(	pef_clause_query([predicate=PredID,number=I],C),
	    	I >= Start			
		),
		Cs
	),
	delete_or_shift_clauses_X(Cs,FileRef,0,Deleted).

delete_or_shift_clauses_X([],_FileRef,TotalDeleted,TotalDeleted).
delete_or_shift_clauses_X([C|Cs],FileRef,AlreadyDeleted,TotalDeleted):-
	pef_clause_get(C,[predicate=PredID,number=I,toplevel=TlRef]),
	pef_clause_retractall([predicate=PredID,number=I]),	
    (	pef_toplevel_query([id=TlRef,file=FileRef])
	-> 	Deleted is AlreadyDeleted + 1
	;	Deleted = AlreadyDeleted,
		J is I - Deleted,
		pef_clause_assert([predicate=PredID,number=J,toplevel=TlRef])
	),
	delete_or_shift_clauses_X(Cs,FileRef,Deleted,TotalDeleted).
	
	




%% unload_file(FileRef+,Cx+).
% remove all clauses added by the file.
% delete all predicates defined in the file that are not multifile.
unload_file(FileRef, Cx):-
    cx_program(Cx,PID),
    (	pef_module_definition_query([id=MID,name=MName,file=FileRef])
    ->	unload_module(MName,MID,Cx)
    ;	true
    ),

    repeat,
    	(	first_clause(FileRef,PID,Clause)
    	->	pef_clause_get(Clause,[predicate=OldPredID,number=Num]),
	    	%create_my_own_predicate_version(OldPredID,PredID,Cx),
	    	% as far as I can tell, the predicate should belong to the current program anyway at this point.
	    	% Let's see if I am wrong
	    	(	predicate_owner(OldPredID,PID)
	    	->	OldPredID=PredID
	    	;	spyme,
	    		throw(i_am_wrong(predicate_owner(OldPredID,PID),unload_file(FileRef,Cx)))
	    	),
	    	(	pef_predicate_property_definition_query([predicate=PredID,property=(multifile)])
	    	->	delete_or_shift_clauses(PredID,FileRef,Num,Deleted),
	    		num_clauses(PredID,NumClauses),
	    		NewNumClauses is NumClauses - Deleted,
	    		set_num_clauses(PredID,NewNumClauses)
	    	;	delete_predicate(PredID)
	    	),
	    	fail
    	;	true
    	),
    !.

unload_module(MName,MID,Cx):-
    cx_program(Cx,PID),
    % if the current program uses an extensions of the module, we convert it to an ad-hoc module.
    pef_module_extension_query([program=PID,base=MID,id=ExtID]),
    !,
    debug(interprete(debug), "turning module extension ~w into an ad-hoc module. ~w~n",[ExtID,Cx]),    
	pef_module_extension_retractall([id=ExtID]),
	pef_ad_hoc_module_assert([id=ExtID,name=MName,program=PID]),
	pef_base:retract(pef_type(ExtID,_)),
	pef_base:assert(pef_type(ExtID,pef_ad_hoc_module)).
unload_module(MName,_MID,Cx):-
    % if the current program use the original module, unbind it.
    unbind_module_name(MName,Cx).



create_my_own_predicate_version(Name,Arity,NewPredID,Cx,Cx1):-
    %try to resolve the pred symbol
    get_predicate(Name,Arity,Cx,PredID),
    !,    
	cx_program(Cx,Program),
	cx_module(Cx,CurrentMID),
	module_name(CurrentMID,ModuleName),
	pef_predicate_query([id=PredID,module=DefMID]),
	
	(	DefMID==CurrentMID
	->	(	module_owner(CurrentMID,Program)
		->	NewPredID=PredID,
			Cx=Cx1
		;	pef_type(CurrentMID,pef_module_definition)
		->	extend_module(CurrentMID,NewMID,Cx),
			cx_set_module(Cx,NewMID,Cx1),
			copy_predicate(PredID,Cx1,NewPredID),
			rebind_module_name(ModuleName,NewMID,Cx1)
		;	copy_module(CurrentMID,NewMID,Cx),
			cx_set_module(Cx,NewMID,Cx1),
			rebind_module_name(ModuleName,NewMID,Cx1),
			get_predicate(Name,Arity,Cx1,NewPredID)
		)
	% If the modules are not identical, we must make sure they are based on the
	% same original definition. Otherwise we have a name clash.
	;	module_base(CurrentMID,Base), 
		module_base(DefMID,Base2),
		Base \== Base2
	->	throw(predicate_already_imported(PredID,Cx))
	% If the modules are not identical, but share the same base,
	% the current module must be an extension of 
	% the definition module, which must be a module definition.
	;	(	module_owner(CurrentMID,Program)
		->	Cx1 = Cx
		;	copy_module(CurrentMID,NewMID,Cx),
			cx_set_module(Cx,NewMID,Cx1),
			rebind_module_name(ModuleName,NewMID,Cx1)
		),
		copy_predicate(PredID,Cx1,NewPredID)
	).
create_my_own_predicate_version(Name,Arity,NewPredID,Cx,Cx1):-
    % the predicate symbol is currently not bound, so we can 
    % start a new predicate.
    cx_module(Cx,CurrentMID),
    cx_program(Cx,Program),
    module_name(CurrentMID,ModuleName),
    (	module_owner(CurrentMID,Program)
    ->	NewMID=CurrentMID,
    	Cx=Cx1
    ;   pef_type(CurrentMID,pef_module_definition)
    ->	extend_module(CurrentMID,NewMID,Cx),
		cx_set_module(Cx,NewMID,Cx1),		
		rebind_module_name(ModuleName,NewMID,Cx1)
	;	copy_module(CurrentMID,NewMID,Cx),
		cx_set_module(Cx,NewMID,Cx1),
		rebind_module_name(ModuleName,NewMID,Cx1)		
	),
	pef_reserve_id(pef_predicate,NewPredID),
    pef_predicate_assert([id=NewPredID,module=CurrentMID,name=Name,arity=Arity]).


%deprecated		  
create_my_own_predicate_version(PredID,NewPredID,Cx):-
    pef_predicate_query([id=PredID,name=Name, arity=Arity,module=MID]),
    cx_program(Cx,Program),    
    module_name(MID,ModuleName),
    (	module_owner(MID,Program)
	    ->  % all green, we own the module (and therefor the predicate),
		NewPredID=PredID	
    ;	% not ours. We must not modify it, but have to create a new module and work on a copy of the predicate
    	% if the module is a real module definition, extend it
    	pef_module_definition_query([id=MID])
    ->	extend_module(MID,NewMID,Cx),
    	cx_set_module(Cx,NewMID,Cx1),
		rebind_module_name(ModuleName,NewMID,Cx1),    	
    	%copy_predicate(PredID,Cx1,NewPredID)
    	pef_predicate_query([id=PredID,name=Name, arity=Arity]),

    	cx_module(Cx1,MID1),
    	pef_reserve_id(pef_predicate,NewPredID),
    	pef_predicate_assert([id=NewPredID,module=MID1,name=Name,arity=Arity]),
    	debug(interprete(debug),"created predicate ~w (~w/~w) in module ~w. ~w~n",[NewPredID,Name,Arity,MID1,Cx1])
	;	% if the module is ad-hoc defined,or a module extension, clone it.
		copy_module(MID,NewMID,Cx),
		pef_predicate_query([id=NewPredID,name=Name, arity=Arity,module=NewMID]),
    	cx_set_module(Cx,NewMID,Cx1),	
		rebind_module_name(ModuleName,NewMID,Cx1)
    ).

copy_predicate(PredID,Cx,NewPredID):-
    pef_predicate_query([id=PredID,name=Name, arity=Arity]),
    cx_module(Cx,NewMID),    
	pef_predicate_query([id=PredID,name=Name, arity=Arity]),
	(	pef_predicate_query([id=NewPredID,name=Name, arity=Arity,module=NewMID])
	->	throw(conflicting_predicate_exists(NewPredID))
	;	pef_reserve_id(pef_predicate,NewPredID),
		debug(interprete(debug),"creating a copy(~w) of predicated(~w) in module ~w. ~w~n",[NewPredID,PredID,NewMID,Cx]),
		pef_predicate_assert([id=NewPredID,name=Name, arity=Arity,module=NewMID]),				
		merge_predicates(append,PredID,NewPredID,Cx)
		
	).
	
    	


get_predicate(Name,Arity,Cx,PredID):-
	cx_module(Cx,MID),
	cx_program(Cx,PID),
	resolve_predicate(PID,MID,Name,Arity,PredID).
    