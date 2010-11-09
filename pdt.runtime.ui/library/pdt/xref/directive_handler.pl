:- module(directive_handler,[handle_directive/6]).

:- ensure_loaded(parse_util).

handle_directive(op,Args,Pos,ParentId,FileId,Module):-
	!,									% operators
	%format('op(~w)~n',[Args]),	
	Call =.. [op|Args],
	Args = [Precedence, Type, Name],
	(	member(Type,[xfx,  xfy, yfx,  yfy])
	->	Arity = 2
	;	Arity = 1
	),
	Pos = term_position(From, To, _FFrom, _FTo, _SubPos),
	assert_new_node(Call,From,To,Id), 
%	directiveT(ParentId,FileId,Module),			
	assert(operatorT(Id,ParentId,FileId,Module,Name,Arity,Type,Precedence)),
	Call.

handle_directive(assert,[file_search_path(Name,Path)],_Pos,ParentId,_FileId,_Module):-
    !,				
	assert(	file_search_path(Name,Path)),
    Path =.. [OtherLib|NewPath],				
    (	NewPath == []		
    ->	LibDir = Path
    ;	(	file_search_path(OtherLib,OtherPath),     % <-- das kann schiefgehen, wenn noch nicht bekannt!!!!!
    		atom_concat(OtherPath,'/',OtherDir),	  % evtl. die relative Library speichern? Prolog macht es ja auch relativ
    		atom_concat(OtherDir,NewPath,LibDir)	
    	)
    ),
    assert(library_dir(Name,LibDir,ParentId)).		 
    
handle_directive(Other,Args,Pos,ParentId,_FileId,_Module):-
%    writeln(Other),
   categorize_directive(Other,Args,Pos,ParentId).
    															%TODO: es können mehrere Anweisungen in einer Direktiven-Klausel sein.
categorize_directive(load_files,Args,_Pos,ParentId):-									
    !,												%TODO: Test this
    nth1(1,Args,Files),	
	(	nth1(2,Args,SomeArgs)
	->	(	member(imports(Imports),SomeArgs),
			(	member(reexport(true),SomeArgs)
			->	assert(export_dir(Files,ParentId))
    		;	true
    		)
		)
	;	Imports=[]							
	),									
    assert(load_dir(ParentId,Files,Imports)).
categorize_directive(module,[NewModule, Exports],_Pos,ParentId):-
    !,
    nb_setval(module_to_parse, NewModule),
    assert(export_dir(Exports,ParentId)).		
categorize_directive(use_module,Args,_Pos,ParentId):-
	!,			
	nth1(1,Args,Files),	
	(	nth1(2,Args,Imports)
	;	Imports = all							
	),
	assert(load_dir(ParentId,Files,Imports)).
categorize_directive(ensure_loaded,Args,_Pos,ParentId):-
	!,					
	assert(load_dir(ParentId,Args,all)).			
categorize_directive(export,Args,_Pos,ParentId):-
    !,
    assert(export_dir(Args,ParentId)).
categorize_directive(reexport,Args,_Pos,ParentId):-
    !,
    nth1(1,Args,Files),	
	(	(	nth1(2,Args,Imports), ! )
	;	Imports = all
	),
    assert(load_dir(ParentId,Files,Imports)),
    assert(export_dir(Files,ParentId)).					
categorize_directive(import,Args,_Pos,ParentId):-
    !,															% <-- muss behandlet werden
    assert(import_dir(Args,ParentId)).
categorize_directive(Functor,Args,_Pos,ParentId):-				% kann das evtl mit use_module zusammen?
    (	Functor == consult
    ;	Functor == include				
    ;	Functor == '.'		
    ),										
    !,
    assert(load_dir(ParentId,Args,all)).
categorize_directive(Functor,Args,Pos,ParentId):-
    (	Functor == (multifile)
    ;   Functor == (dynamic)
    ;	Functor == (module_transparent)
    ;	Functor == (discontiguous)
    ;	Functor == (meta_predicate)
    ),
    !,
    assert(property_dir(Functor,Args,ParentId,Pos)).
/*categorize_directive(Functor,_Args,_Pos,_FileId,_ParentId,_Module):-
 %   assert(other_dir(FileId,Args,ParentId,Pos)).        <-- hier evtl noch ausbauen bzw etwas speichern    
   writeln(Functor).*/
categorize_directive(_Functor,_Args,_Pos,_ParentId).

   