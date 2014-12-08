/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( pdt_search,
         [ find_predicate_reference/11                  % (+Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?PropertyList)
         , find_categorized_predicate_definitions/11       % (+EnclFile,+SelectionLine, +Term, -Functor, -Arity, -This, -DeclOrDef, -DefiningEntity, -FullPath, -Line, -Properties,-Visibility,+ExactMatch)
         , find_predicate_definitions/10
         , find_primary_definition_visible_in/7  % (EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult)
         , find_definition_contained_in/9
         , find_definition_contained_in/10
         , find_completion/13
         , find_entity_definition/6
         , find_module_reference/9
         , find_alternative_predicates/7
         , loaded_file/1
         , loaded_by/4
         ]).

:- use_module( prolog_connector_pl(split_file_path),
             [ split_file_path/5                % (File,Folder,FileName,BaseName,Extension)
             ] ).
:- use_module( 'xref/pdt_xref', 
             [ find_reference_to/9             % ...
             ] ).
:- use_module( properties, 
             [ properties_for_predicate/4
             ] ).
%:- use_module( pdt_prolog_library(utils4modules_visibility),
%             [ module_of_file/2                 % (File,FileModule)
%             , defined_in_module/3              % (Module,Name,Arity),
%             , declared_in_file/4               % (Module,Name,Arity,Location)
%             , defined_in_files/4               % (Module,Name,Arity,Locations)
%             ] ).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(pdt_manual_entry).
:- use_module(library(charsio)). 
:- use_module(library(lists)). 
:- use_module(library(prolog_clause)).
:- use_module(pdt_prolog_library(compatibility), [
	pdt_source_file/2
]).

:- op(600, xfy, ::).   % Logtalk message sending operator

%% find_predicate_reference(Term, ExactMatch, Root, EnclFile, LineInEnclFile, RefModule, RefName, RefArity, RefFile, Position, PropertyList)

find_predicate_reference(Term, ExactMatch, Root, _EnclFile, _LineInEnclFile, RefModule, RefName, RefArity, RefFile, Position, PropertyList) :-
	current_predicate(logtalk_load/1),
	logtalk_adapter::find_reference_to(Term, ExactMatch, Root, RefModule, RefName, RefArity, RefFile, Position, PropertyList).
find_predicate_reference(Term0, ExactMatch, Root, EnclFile, _LineInEnclFile, RefModule, RefName, RefArity, RefFile, Position, PropertyList) :-
	(	nonvar(EnclFile),
		Term0 = predicate(M, S0, F, S1, A),
		var(M)
	->	once(module_of_file(EnclFile, FileModule)),
		(	nonvar(F),
			nonvar(A),
			functor(H, F, A),
			predicate_property(FileModule:H, imported_from(ImportedModule))
		->	Term = predicate(ImportedModule, S0, F, S1, A)
		;	Term = predicate(FileModule, S0, F, S1, A)
		)
	;	Term = Term0
	),
	find_reference_to(Term, ExactMatch, Root, RefModule, RefName, RefArity, RefFile, Position, PropertyList).

        /***********************************************************************
         * Find Definitions and Declarations and categorize them by visibility *
         * --------------------------------------------------------------------*
         * for "Find All Declarations" (Ctrl+G) action                         *
         ***********************************************************************/ 

%% find_predicate_definitions(+Term, +ExactMatch, ?Root, -DefiningModule, -Functor, -Arity, -DeclOrDef, -File, -Location, -Properties) is nondet.
% 
find_predicate_definitions(Term, ExactMatch, Root, DefiningModule, Functor, Arity, DeclOrDef, File, Location, PropertyList) :-
	Term = predicate(DefiningModule, _, SearchFunctor, Separator, Arity0),
	(	Separator == (//),
		nonvar(Arity0)
	->	Arity is Arity0 + 2
	;	Arity = Arity0
	),
	(	ExactMatch == true
	->	Functor = SearchFunctor,
		declared_in_module(DefiningModule, Functor, Arity, DefiningModule)
	;	declared_in_module(DefiningModule, Functor, Arity, DefiningModule),
		once(sub_atom(Functor,_,_,_,SearchFunctor))
	),
    find_decl_or_def_2(Functor,Arity,Sources),              % Unique, grouped sources (--> setof)
    member(DeclOrDef-DefiningModule-Locations,Sources),
    member(File-Lines,Locations),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	),
    member(location(Line, Ref),Lines),
    properties_for_predicate(DefiningModule,Functor,Arity,PropertyList0),
    (	head_position_of_clause(Ref, Position)
    ->	Location = Position,
    	PropertyList = [line(Line)|PropertyList0]
    ;	Location = Line,
    	PropertyList = PropertyList0
    ).

find_predicate_definitions(Term, ExactMatch, Root, DefiningModule, Functor, Arity, DeclOrDef, File,Line, PropertyList) :-
	current_predicate(logtalk_load/1),
	logtalk_adapter::find_predicate_definitions(Term, ExactMatch, Root, DefiningModule, Functor, Arity, DeclOrDef, File,Line, PropertyList).


%% find_categorized_predicate_definitions(+Term, +EnclFile, +SelectionLine, Functor, Arity, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility)

find_categorized_predicate_definitions(Term, EnclFile, SelectionLine, Functor, Arity, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility):-
    Term \= _:_,
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_categorized_predicate_definitions(Term, EnclFile, SelectionLine, Functor, Arity, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility).

find_categorized_predicate_definitions(Term, EnclFile, _SelectionLine, Functor, Arity, DeclOrDef, DefiningModule, File, Location, PropertyList, Visibility):-
    referenced_entity(EnclFile, ReferencedModule),    
    Term = predicate(_, _, Functor, _, Arity),
    find_decl_or_def(ReferencedModule,Functor,Arity,Sources),              % Unique, grouped sources (--> setof)
    member(DeclOrDef-Visibility-DefiningModule-Locations,Sources),
    member(File-Lines,Locations),
    member(location(Line, Ref),Lines),
    properties_for_predicate(DefiningModule,Functor,Arity,PropertyList0),
    (	head_position_of_clause(Ref, Position)
    ->	Location = Position,
    	PropertyList = [line(Line)|PropertyList0]
    ;	Location = Line,
    	PropertyList = PropertyList0
    ).

head_position_of_clause(Ref, Position) :-
	catch(clause_info(Ref, _, TermPosition, _),_,fail),
	(	clause_property(Ref, fact)
	->	% fact
		TermPosition = HeadPosition
	;	% clause with body
		TermPosition = term_position(_, _, _, _, [HeadPosition|_])
	),
	(	HeadPosition = Start-End
	->	% no arguments
		true
	;	% at least one argument
		HeadPosition = term_position(Start, End, _, _, _)
	),
	format(atom(Position), '~w-~w', [Start, End]).

find_decl_or_def_2(Name,Arity,Declarations) :-
   setof( declaration-DeclModule-Location, Name^Arity^ 
          ( declared_but_undefined(DeclModule,Name,Arity),
            declared_in_file(DeclModule,Name,Arity,Location)
          ),
          Declarations).
    
find_decl_or_def_2(Name,Arity,Definitions) :-
   setof( definition-DefiningModule-Locations, Name^Arity^  % Locations is list of File-Lines terms
          ( defined_in_module(DefiningModule,Name,Arity),
            defined_in_files(DefiningModule,Name,Arity,Locations)
%            results_context_category_label(defined, Visibility, VisibilityText)
          ),
          Definitions
    ). 

referenced_entity(EnclFile, ReferencedModule) :- 
    (  atom(ReferencedModule)
    -> true                            % Explicit entity reference ReferencedModule:Term (or ReferencedModule::Term
    ;  module_of_file(EnclFile,ReferencedModule)   % Implicit module reference
    ).

search_term_to_predicate_indicator(_:Functor/(-1), Functor/_Arity) :- !.
search_term_to_predicate_indicator(Functor/(-1), Functor/_Arity) :- !.
search_term_to_predicate_indicator(_:Functor/Arity, Functor/Arity) :- !.
search_term_to_predicate_indicator(Functor/Arity, Functor/Arity) :- !.
search_term_to_predicate_indicator(_:Term, Functor/Arity) :- !, functor(Term, Functor, Arity).
search_term_to_predicate_indicator(Term, Functor/Arity) :- functor(Term, Functor, Arity).


%% find_decl_or_def(+ContextModule,+Name,?Arity,-Visibility,-Sources)

find_decl_or_def(Module,Name,Arity,Sources) :-
    ( var(Module)
    ; var(Name)
    ),
    throw( input_argument_free(find_decl_or_def(Module,Name,Arity,Sources)) ).

%find_decl_or_def(CallingModule,Name,Arity,['Missing declarations'-DeclModule-[File-[Line]]]) :-
%   referenced_but_undeclared(CallingModule,Name,Arity),
%   DeclModule = 'none',
%   File = 'none',
%   Line = 0.
find_decl_or_def(CallingModule,Name,Arity,[declaration-none-none-[none-[none]]]) :-
   referenced_but_undeclared(CallingModule,Name,Arity).
      
find_decl_or_def(ContextModule,Name,Arity,Declarations) :-
   setof( declaration-Visibility-DeclModule-Location, ContextModule^Name^Arity^ 
          ( declared_but_undefined(DeclModule,Name,Arity),
            visibility(Visibility, ContextModule,Name,Arity,DeclModule),
            declared_in_file(DeclModule,Name,Arity,Location)
%            results_context_category_label(declared, Visibility, VisibilityText)
          ),
          Declarations).
    
find_decl_or_def(ContextModule,Name,Arity,Definitions) :-
   setof( definition-Visibility-DefiningModule-Locations, ContextModule^Name^Arity^  % Locations is list of File-Lines terms
          ( defined_in_module(DefiningModule,Name,Arity),
            visibility(Visibility, ContextModule,Name,Arity,DefiningModule),
            defined_in_files(DefiningModule,Name,Arity,Locations)
%            results_context_category_label(defined, Visibility, VisibilityText)
          ),
          Definitions
    ). 

:- multifile(results_category_label/2).


:- multifile(results_context_category_label/3).
%results_context_category_label(declared, local,      'Local declaration' ) :- !.
%results_context_category_label(declared, supermodule,'Imported declaration' ) :- !.
%results_context_category_label(declared, submodule,  'Submodule declaration') :- !.
%results_context_category_label(declared, invisible,  'Locally invisible declaration') :- !.
%
%results_context_category_label(defined, local,      'Local definitions' ) :- !.
%results_context_category_label(defined, supermodule,'Imported definitions' ) :- !.
%results_context_category_label(defined, submodule,  'Submodule definitions') :- !.
%results_context_category_label(defined, invisible,  'Locally invisible definitions') :- !.

    
% These clauses must stay in this order since they determine
% the order of elements in the result of  find_decl_or_def
% and hence the order of elements in the search result view
% (which is the INVERSE of the order of elements that we
% compute here).               
         

visibility(super, ContextModule,Name,Arity,DeclModule) :-
	(	module_imports_predicate_from(ContextModule, Name, Arity, DeclModule, overridden)
	;	module_imports_predicate_from(ContextModule, Name, Arity, DeclModule, imported)
	),
	!.
%    declared_in_module(ContextModule,Name,Arity,DeclModule),
%    ContextModule \== DeclModule. 
    
   
visibility(local, ContextModule,Name,Arity,DeclModule) :-
    declared_in_module(ContextModule,Name,Arity,DeclModule),
    ContextModule == DeclModule,
    !.
    
visibility(sub, ContextModule,Name,Arity,DeclModule) :-
	(	module_imports_predicate_from(DeclModule, Name, Arity, ContextModule, overridden)
	;	module_imports_predicate_from(DeclModule, Name, Arity, ContextModule, imported)
	),
	!.
%    declared_in_module(DeclModule,Name,Arity,DeclModule),
%    % DeclModule is a submodule of ContextModule
%    declared_in_module(DeclModule,_,_,ContextModule), % submodule
%    ContextModule \== DeclModule. 
visibility(invisible, ContextModule,Name,Arity,DeclModule) :-
    % Take care to generate all values befor using negation.
    % Otherwise the clause will not be able to generate values.
    % Negation DOES NOT generate values for unbound variables!
    
    % There is some DeclaringModule
    declared_in_module(DeclModule,Name,Arity,DeclModule),
    ContextModule \== DeclModule,
	\+ module_imports_predicate_from(DeclModule, Name, Arity, ContextModule, overridden),
	\+ module_imports_predicate_from(DeclModule, Name, Arity, ContextModule, imported),
	\+ module_imports_predicate_from(ContextModule, Name, Arity, DeclModule, overridden),
	\+ module_imports_predicate_from(ContextModule, Name, Arity, DeclModule, imported).
%    \+ declared_in_module(ContextModule,Name,Arity,DeclModule),
%    \+ declared_in_module(DeclModule,_,_,ContextModule).

        /***********************************************************************
         * Find Primary Definition                                             *
         * --------------------------------------------------------------------*
         * for "Open Primary Declaration" (F3) action                          *
         ***********************************************************************/ 

%% find_primary_definition_visible_in(+EnclFile,+SelectedLine,+TermString,?ReferencedModule,?MainFile,?FirstLine,?ResultKind)
%
% Find first line of first clause in the *primary* file defining the predicate Name/Arity 
% visible in ReferencedModule. In case of multifile predicates, the primary file is either 
% the file whose module is the DefiningModule or otherwise (this case only occurs
% for "magic" system modules, (e.g. 'system')) the file containing most clauses.
%
% Used for the open declaration action in 
% pdt/src/org/cs3/pdt/internal/actions/FindPredicateActionDelegate.java
% 
% ResultKind is one of: single, multifile, foreign, dynamic
        
find_primary_definition_visible_in(EnclFile,SelectedLine,TermString,ReferencedModule,MainFile,FirstLine,ResultKind) :-
	retrieve_term_from_atom(EnclFile, TermString, Term),
	find_primary_definition_visible_in_(EnclFile,SelectedLine,Term,ReferencedModule,MainFile,FirstLine,ResultKind).

retrieve_term_from_atom(EnclFile, TermString, Term) :-
	(	module_property(Module, file(EnclFile))
	->	atom_concat(TermString, '.', TermStringWithDot),
		open_chars_stream(TermStringWithDot, Stream),
		read_term(Stream, Term, [module(Module)])
	;	atom_to_term(TermString, Term, _)
	).

find_primary_definition_visible_in_(EnclFile, SelectedLine,Term,ReferencedModule,MainFile,FirstLine,single) :-
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    current_predicate(logtalk_load/1),
    logtalk_adapter::find_primary_definition_visible_in(EnclFile, SelectedLine,Term,_Functor,_Arity,ReferencedModule,MainFile,FirstLine).

% The second argument is just an atom contianing the string representation of the term:     
find_primary_definition_visible_in_(_EnclFile,_SelectedLine,Term,_ReferencedModule,MainFile,FirstLine,single) :-
	Term = Obj::Call,
	!,
    current_predicate(logtalk_load/1),
    nonvar(Obj),
    nonvar(Call),
    functor(Call, Name, Arity),
	(	object_property(Obj, defines(Name/Arity, Properties))
	;	object_property(Obj, declares(Name/Arity, Properties))
	),
	memberchk(line_count(FirstLine), Properties),
	object_property(Obj, file(FileName, Directory)),
	atom_concat(Directory, FileName, MainFile),
	!.
    	
find_primary_definition_visible_in_(EnclFile,_SelectedLine,Term,ReferencedModule,MainFile,FirstLine,ResultKind) :-
    extract_name_arity(Term, _,_Head,Name,Arity),
    find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine,ResultKind).

extract_name_arity(Term,Module,Head,Name,Arity) :-
	(	var(Term) 
	->	throw( 'Cannot display the definition of a variable. Please select a predicate name.' )
	;	true
	),
	% Special treatment of Name/Arity terms:
	(	Term = Module:Name/Arity
	->	true
	;	(	Term = Name/Arity
		->	true
		;	(	Term = Module:Term2
			->	functor(Term2, Name, Arity)
			;	functor(Term,Name,Arity)
			)
		)
	),
	% Create most general head
	functor(Head,Name,Arity).

% Now the second argument is a real term that is 
%  a) a file loading directive:     
find_primary_definition_visible_in__(EnclFile,Term,_,_,_,File,Line,single):-
    find_file(EnclFile,Term,File,Line).

%  b) a literal (call or clause head):    
find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine,ResultKind) :-
	find_definition_visible_in(EnclFile,Term,Name,Arity,ReferencedModule,DefiningModule,Locations),
	(	Locations = [_,_|_]
	->	ResultKind = (multifile)
	;	Locations = [Location],
		(	Location = (dynamic)-_ ->
			ResultKind = (dynamic)
		; 	Location = foreign-_ -> 
			ResultKind = foreign
		; 	ResultKind = single
		)
	),
	primary_location(Locations,DefiningModule,MainFile,FirstLine).


% If Term is a loading directive, find the related file,
% eventually interpreting a FileSPec that contains an alias
find_file(EnclFile,Term,File,Line) :-
    extract_file_spec(Term,FileSpec),
    catch( absolute_file_name(FileSpec,[relative_to(EnclFile), solutions(all),extensions(['.pl', '.lgt', '.ct', '.ctc'])], File),
           _,
           fail
    ),
    access_file(File, read),
    !,
    Line=1.
    
% Work regardelessly whether the user selected the entire consult/use_module
% statement or just the file spec. Does NOT work if he only selected a file
% name within an alias but not the complete alias.
extract_file_spec(consult(FileSpec),FileSpec) :- !.
extract_file_spec(use_module(FileSpec),FileSpec) :- !.
extract_file_spec(use_module(FileSpec,_),FileSpec) :- !.
extract_file_spec(reexport(FileSpec),FileSpec) :- !.
extract_file_spec(reexport(FileSpec,_),FileSpec) :- !.
extract_file_spec(ensure_loaded(FileSpec),FileSpec) :- !.
extract_file_spec(Term,Term).
    
find_definition_visible_in(EnclFile,_Term,Name,Arity,ReferencedModule,DefiningModule,Locations) :-
	module_of_file(EnclFile,FileModule),
	(	atom(ReferencedModule)
	->	true                            % Explicit module reference
	;	ReferencedModule = FileModule   % Implicit module reference
	),
	(	defined_in_module(ReferencedModule,Name,Arity,DefiningModule)
	->	defined_in_files(DefiningModule,Name,Arity,Locations0),
		(	Locations0 == []
		->	declared_in_file(DefiningModule, Name, Arity, Locations)
		;	Locations0 = Locations
		)
	;	declared_in_module(ReferencedModule,Name,Arity,DeclaringModule),
		defined_in_files(DeclaringModule,Name,Arity,Locations0),
		(	Locations0 == []
		->	declared_in_file(DefiningModule, Name, Arity, Locations)
		;	Locations0 = Locations
		)
	).

primary_location(Locations,DefiningModule,File,FirstLine) :-
    member(File-Lines,Locations),
    module_of_file(File,DefiningModule),
    !,
    Lines = [location(FirstLine, _Ref)|_].
primary_location(Locations,_,File,FirstLine) :-
    findall( NrOfClauses-File-FirstLine,
             ( member(File-Lines,Locations),
               length(Lines,NrOfClauses),
               Lines=[location(FirstLine, _Ref)|_]
             ),
             All
    ),
    sort(All, Sorted),
    Sorted = [ NrOfClauses-File-FirstLine |_ ].
    

:- if(current_prolog_flag(dialect, swi)).
find_alternative_predicates(EnclFile, TermString, RefModule, RefName, RefArity, RefFile, RefLine) :-
	retrieve_term_from_atom(EnclFile, TermString, Term),
	extract_name_arity(Term,Module, Head,_Name,_Arity),
	(	var(Module)
	->	once(module_of_file(EnclFile,FileModule)),
		dwim_predicate(FileModule:Head, RefModule:RefHead)
	;	dwim_predicate(Module:Head, RefModule:RefHead)
	),
	functor(RefHead, RefName, RefArity),
	(	predicate_property(RefModule:RefHead, file(RefFile)),
		predicate_property(RefModule:RefHead, line_count(RefLine))
	->	true
	;	RefFile = foreign,
		RefLine = -1
	).
:- else.
find_alternative_predicates(_EnclFile, _TermString, _RefModule, _RefName, _RefArity, _RefFile, _RefLine) :-
	fail.
:- endif.

        /***********************************************************************
         * Find Definitions in File                                            *
         * --------------------------------------------------------------------*
         * for Outline                                                         *
         ***********************************************************************/ 
         
% TODO: This is meanwhile subsumed by other predicates. Integrate!
   
%% find_definition_contained_in(+File, +Options, -Entity, -EntityLine, -EntityKind, -Name, -Arity, -SearchCategory, -Line, -PropertyList) is nondet.
%
% Look up the starting line of a clause of a predicate Name/Arity defined in File. 
% Do this upon backtracking for each clause of each predicate in File.
%
% @param File           The file to search for definitions.
% @param Options        multifile(boolean): show clauses of other files for multifile predicates
%                       all_clauses(boolean): show all clauses or only the first of a predicate in a file
% @param Entity         The module (in Prolog) or Entity (in logtalk) to which a predicate belongs.
%                       Note that there can be multiple entities in a file. 
% @param EntityKind     "module" (in Prolog) or "object|category|protocoll" (in Logtalk)
% @param Name           The name of the defined predicate
% @param Arity          The arity of the defined predicate
% @param SearchCategory "multifile|definition"
% @param Line           The starting line of a clause of Name/Arity contained in File.
% @param PropertyList   A list of properties of the predicate. 
%                       TODO: Enumerate values expected by the Java GUI.

% Called from PDTOutlineQuery.java

find_definition_contained_in(File, Entity, EntityLine, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList) :-
	find_definition_contained_in(File, [multifile(true), all_clauses(true)], Entity, EntityLine, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList).

find_definition_contained_in(File, Options, Entity, EntityLine, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList) :-
    split_file_path(File, _Directory,_FileName,_,lgt),
    !,
    current_predicate(logtalk_load/1),
    logtalk_adapter::find_definition_contained_in(File, Options, Entity, EntityLine, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList).

find_definition_contained_in(ContextFile, Options, DefiningModule, ModuleLine, module, Functor, Arity, SearchCategory, Line, PropertyList) :-
    SearchCategory = definition,
    
%    module_of_file(ContextFile, ContextModule),
    % Backtrack over all predicates defined in File
    % including multifile contributions to other modules:
    (	pdt_source_file(DefiningModule:Head, ContextFile)
    ;	source_file_property(ContextFile, included_in(ParentFile, _)),
    	pdt_source_file(DefiningModule:Head, ParentFile)
    ),
    (	module_property(DefiningModule, line_count(ModuleLine))
    ->	true
    ;	ModuleLine = 1
    ),
    
    % Predicate properties:
    functor(Head, Functor, Arity),
    \+ find_blacklist(Functor,Arity,DefiningModule),
    properties_for_predicate(DefiningModule,Functor, Arity, PropertyList0),

    % In the case of a multifile predicate, we want to find all clauses for this 
    % predicate, even when they occur in other files
    %defined_in_file(DefiningModule, Functor, Arity, Ref, _, DefiningFile, Line),
    find_definition_in_file(Options, ContextFile, DefiningModule, Functor, Arity, Ref, DefiningFile, Line),
    (	nonvar(ParentFile)
    ->	DefiningFile \== ParentFile
    ;	true
    ),
    (	DefiningFile == ContextFile
    ->	(	module_of_file(ContextFile, DefiningModule)%DefiningModule == ContextModule
    	->	% local definition
    		PropertyList1 = [local(DefiningModule)|PropertyList0]
	    ;	% contribution of ContextModule for DefiningModule	
    		PropertyList1 = [for(DefiningModule), defining_file(DefiningFile) | PropertyList0]
    	)
    ;	(	module_of_file(ContextFile, DefiningModule)%DefiningModule == ContextModule
    	->	% contribution from DefiningFile to ContextModule, ContextFile
    		PropertyList1 = [from(DefiningModule), defining_file(DefiningFile) | PropertyList0]
    	;	% other file to itself
    		PropertyList1 = [remote(DefiningModule), defining_file(DefiningFile) | PropertyList0]
    	)
    ),
    (	first_argument_of_clause(Ref, FirstArg)
    ->	PropertyList = [FirstArg|PropertyList1]
    ;	PropertyList = PropertyList1
    ).

find_definition_in_file(Options, ContextFile, Module, Name, Arity, Ref, File, Line) :-
	once(member(multifile(IsMultifile), Options)),
	once(member(all_clauses(IsAllClauses), Options)),
	find_definition_in_file(IsAllClauses, IsMultifile, ContextFile, Module, Name, Arity, Ref, File, Line).

find_definition_in_file(true, IsMultifile, ContextFile, Module, Name, Arity, Ref, File, Line) :-
	!,
	(	IsMultifile == true
	->	true
	;	ContextFile = File
	),
	defined_in_file(Module, Name, Arity, Ref, _, File, Line).
find_definition_in_file(_/*false*/, IsMultifile, ContextFile, Module, Name, Arity, Ref, File, Line) :-
	functor(Head, Name, Arity),
	(	predicate_property(Module:Head, multifile)
	->	(	IsMultifile == true
		->	true
		;	ContextFile = File
		),
		find_lines(Module:Head, File, Line, Ref)
	;	ContextFile = File,
		predicate_property(Module:Head, line_count(Line)),
		nth_clause(Module:Head, _, Ref),
		clause_property(Ref, line_count(Line)),
		!
	).

:- dynamic(file_line/3).
find_lines(Head, File, Line, Ref) :-
	predicate_property(Head, number_of_clauses(N)), 
	N > 1000,
	!,
	findall(F, (source_file(F), pdt_source_file(Head, F)), Files),
	Line = 1,
	Ref = [],
	member(File, Files).
find_lines(Head, File, Line, Ref) :-
	with_mutex(find_lines, (
		retractall(file_line(_, _, _)),
		walk_clauses(Head, 1),
		findall(F-L-R, file_line(F, L, R), Locations)
	)),
	member(File-Line-Ref, Locations).
walk_clauses(Head, N) :-
	nth_clause(Head, N, Ref),
	!,
	(	clause_property(Ref, file(File)),
		clause_property(Ref, line_count(Line))
	->	(	file_line(File, L, _)
		->	(	Line < L
			->	retract(file_line(File, L, _)),
				assertz(file_line(File, Line, Ref))
			;	true
			)
		;	assertz(file_line(File, Line, Ref))
		)
	;	true
	),
	succ(N, N2),
	walk_clauses(Head, N2).
walk_clauses(_, _).

first_argument_of_clause(Ref, first_argument(Arg)) :-
	catch(clause(_:Head, _, Ref), _, fail),
	compound(Head),
	arg(1, Head, FirstArg),
	(	var(FirstArg)
	->	clause_info(Ref, _, _, Varnames), 
		arg(1, Varnames, FirstVarName),
		Arg = FirstVarName
	;	functor(FirstArg, F, N),
		(	N == 0
		->	Arg = F
		;	format(atom(Arg), '~w/~w', [F, N])
		)
	).

% TODO: Reconcile the above with utils4modules_visibility.pl::module_of_file/2   

%% find_blacklist(?Functor, ?Arity, ?Module) is nondet.
%
% Used to remove (internal) predicates from the results of find_definition_contained_in/8.
%
%
find_blacklist('$load_context_module',2,_).
find_blacklist('$load_context_module',3,_).
find_blacklist('$mode',2,_).
find_blacklist('$pldoc',4,_).

    
    


               /***********************************************
                * FIND VISIBLE PREDICATE (FOR AUTOCOMPLETION) *
                ***********************************************/

%% find_completion(+Prefix, ?EnclosingFile, ?LineInFile, -Kind, -Entity, -Name, -Arity, -Visibility, -IsBuiltin, -IsDeprecated, -ArgNames, -DocKind, -Doc) is nondet.
% 
find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	var(Prefix),
	!,
	throw(prefix_not_bound(find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc))).

find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	nonvar(EnclosingFile),
	!,
	(	split_file_path(EnclosingFile, _, _, _, lgt)
	->	current_predicate(logtalk_load/1),
		IsDeprecated = false,
		logtalk_adapter::find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, ArgNames, DocKind, Doc)
	;	find_completion_(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc)
	).

find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	(	find_completion_(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc)
	;	current_predicate(logtalk_load/1),
		IsDeprecated = false,
		logtalk_adapter::find_completion(Prefix, EnclosingFile, LineInFile, Kind, Entity, Name, Arity, Visibility, IsBuiltin, ArgNames, DocKind, Doc)
	).

:- discontiguous(find_completion_/13).

find_completion_(SpecifiedModule:PredicatePrefix, _EnclosingFile, _LineInFile, predicate, Module, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	!,
	(	PredicatePrefix \== ''
	->	setof(
			Module-Name-Arity,
			SpecifiedModule^(
				declared_in_module(SpecifiedModule, Name, Arity, Module),
				is_case_insensitive_prefix_of(PredicatePrefix, Name)
			),
			Predicates
		)
	;	nonvar(SpecifiedModule),
		setof(
			SpecifiedModule-Name-Arity,
			SpecifiedModule^(declared_in_module(SpecifiedModule, Name, Arity, SpecifiedModule)),
			Predicates
		)
	),
	member(Module-Name-Arity, Predicates),
	predicate_information(Module, Name, Arity, IsBuiltin, IsDeprecated, Visibility, ArgNames, DocKind, Doc).

find_completion_(PredicatePrefix, EnclosingFile, _LineInFile, predicate, Module, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	atomic(PredicatePrefix),
	nonvar(EnclosingFile),
	setof(
		Module-Name-Arity,
		EnclosingFile^FileModule^IncludeLine^File^(
			(	File = EnclosingFile
			;	source_file_property(EnclosingFile, included_in(File, IncludeLine))
			),
			module_of_file(File, FileModule),
			declared_in_module(FileModule, Name, Arity, Module),
			is_case_insensitive_prefix_of(PredicatePrefix, Name)
		),
		Predicates
	),
	member(Module-Name-Arity, Predicates),
	predicate_information(Module, Name, Arity, IsBuiltin, IsDeprecated, Visibility, ArgNames, DocKind, Doc).

find_completion_(PredicatePrefix, EnclosingFile, _LineInFile, predicate, Module, Name, Arity, Visibility, IsBuiltin, IsDeprecated, ArgNames, DocKind, Doc) :-
	atomic(PredicatePrefix),
	var(EnclosingFile),
	setof(Module-Name-Arity, (
		declared_in_module(user, Name, Arity, Module),
		is_case_insensitive_prefix_of(PredicatePrefix, Name)
	), Predicates),
	member(Module-Name-Arity, Predicates),
	predicate_information(Module, Name, Arity, IsBuiltin, IsDeprecated, Visibility, ArgNames, DocKind, Doc).

predicate_information(Module, Name, Arity, IsBuiltin, IsDeprecated, Visibility, ArgNames, DocKind, Doc) :-
	functor(Head, Name, Arity),
	(	predicate_property(Module:Head, built_in)
	->	IsBuiltin = true
	;	IsBuiltin = false
	),
	(	(	Module == user
		;	predicate_property(Module:Head, exported)
		)
	->	Visibility = (public)
	;	Visibility = protected
	),
	(	predicate_completion_documentation_hook(Module, Name, Arity, ArgNames, DocKind, Doc)
	->	true
	;	predicate_manual_entry(Module, Name, Arity, Content, IsDeprecated),
		(	Content == nodoc
		->	DocKind = nodoc
		;	DocKind = html,
			Doc = Content,
			(	Arity == 0
			->	ArgNames = []
			;	ignore(predicate_arg_list(Content, ArgNames))
			)
		)
	),
	(	var(IsDeprecated)
	->	IsDeprecated = false
	;	true
	).

:- multifile(predicate_completion_documentation_hook/6).

predicate_arg_list(Comment, ArgList) :-
	sub_atom(Comment, End, _, _, '</var>'),
	sub_atom(Comment, BeforeBegin, BeforeBeginLength, _, '"arglist">'),
	!,
	Begin is BeforeBegin + BeforeBeginLength,
	Length is End - Begin,
	sub_atom(Comment, Begin, Length, _, Args),
	format(atom(ArgsWithDot), '~w.', [Args]),
	open_chars_stream(ArgsWithDot, Stream),
	call_cleanup(read_term(Stream, Term, [variable_names(Vars), module(pldoc_modes)]), close(Stream)),
	args_from_mode_term(Term, ArgList, Vars).

args_from_mode_term(Arg, [ArgName], Vars) :-
	var(Arg),
	!,
	var_to_arg(Arg, ArgName, Vars).

args_from_mode_term((Arg, Args), [ArgName|ArgNames], Vars) :-
	!,
	var_to_arg(Arg, ArgName, Vars),
	args_from_mode_term(Args, ArgNames, Vars).

args_from_mode_term(Arg, [ArgName], Vars) :-
	var_to_arg(Arg, ArgName, Vars).

var_to_arg(Arg, ArgName, Vars) :-
	(	var(Arg)
	->	Arg = Var
	;	Arg =.. [_Mode|Other],
		(	Other = [Var:_]
		;	Other = [Var]
		)
	),
	member(ArgName=V, Vars),
	V == Var,
	!.

find_completion_(ModulePrefix, _EnclosingFile, _LineInFile, module, _, Name, _, _, _, _, _, _, _) :- 
	atomic(ModulePrefix),
	current_module(Name),
	is_case_insensitive_prefix_of(ModulePrefix,Name).

:- if(current_prolog_flag(dialect, swi)).
find_completion_(AtomPrefix, _EnclosingFile, _LineInFile, atom, _, Atom, _, _, _, _, _, _, _) :- 
	atomic(AtomPrefix),
	garbage_collect_atoms,
	'$atom_completions'(AtomPrefix, Atoms),
	member(Atom,Atoms),
	'$atom_references'(Atom, ReferenceCount),
	ReferenceCount > 0, 
%	Atom \= AtomPrefix,
	\+ current_predicate(Atom/_Arity).
:- endif.

is_case_insensitive_prefix_of(Prefix, Name) :-
	downcase_atom(Prefix, PrefixDowncase),
	downcase_atom(Name, NameDowncase),
	atom_concat(PrefixDowncase, _, NameDowncase).

%% find_entity_definition(SearchString, ExactMatch, Root, File, Line, Entity)

find_entity_definition(SearchString, ExactMatch, Root, File, Line, Entity) :-
	current_predicate(logtalk_load/1),
	logtalk_adapter::find_entity_definition(SearchString, ExactMatch, Root, File, Line, Entity).

find_entity_definition(SearchString, ExactMatch, Root, File, Line, Entity) :-
	find_module_definition(SearchString, ExactMatch, Root, File, Line, Entity).
	

find_module_definition(SearchModule, ExactMatch, Root, File, Line, Module) :-
	current_module(Module),
	(	ExactMatch == true
	->	SearchModule = Module
	;	once(sub_atom(Module, _, _, _, SearchModule))
	),
	module_property(Module, file(File)),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	),
	module_property(Module, line_count(Line)).
	

%% find_module_reference(Module, ExactMatch, Root, File, Line, ReferencingModule, RefName, RefArity, PropertyList)

find_module_reference(Module, ExactMatch, Root, File, Line, system, load_files, 2, [show_line(true)|PropertyList]) :-
	once(properties_for_predicate(system, load_files, 2, PropertyList)),
	find_use_module(Module, ExactMatch, _, _, File, Line),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	).

find_module_reference(Module, ExactMatch, Root, File, Line, ReferencingModule, RefName, RefArity, PropertyList) :-
	search_module_name(Module, ExactMatch, SearchModule),
	find_reference_to(predicate(SearchModule, _, _, _, _), ExactMatch, Root, ReferencingModule, RefName, RefArity, File, Line, PropertyList).

find_module_reference(Module, ExactMatch, Root, File, Line, ReferencingModule, RefName, RefArity, PropertyList) :-
	current_predicate(logtalk_load/1),
	logtalk_adapter::find_entity_reference(Module, ExactMatch, Root, File, Line, ReferencingModule, RefName, RefArity, PropertyList).

search_module_name(Module, true, Module) :- !.
search_module_name(ModulePart, false, Module) :-
	current_module(Module),
	once(sub_atom(Module, _, _, _, ModulePart)).

%% loaded_by(LoadedFile, LoadingFile, Line, Directive)

/* For Load File Dependency Graph: */
loaded_by(LoadedFile, LoadingFile, Line, Directive) :-
	nonvar(LoadedFile),
	(	split_file_path(LoadedFile, _, _, _, lgt)
	;	split_file_path(LoadedFile, _, _, _, logtalk)
	),
	% Logtalk source file
	!,
	logtalk_adapter::loaded_by(LoadedFile, LoadingFile, Line, Directive).

loaded_by(LoadedFile, LoadingFile, Line, Directive) :-
	source_file_property(LoadedFile, load_context(_LoadingModule, LoadingFile:Line, _OptionList)),
	% If the loaded file is a module conclude that the
	% loading directive should have been 'use_module'.
	% Otherwise, treat it as a 'consult': 
	( module_property(_Module, file(LoadedFile))
	-> Directive = use_module
	;  Directive = consult
	).
	
find_use_module(ModuleOrPart, ExactMatch, ModuleFile, LoadingModule, File, Line) :-
	(	ExactMatch == true
	->	ModuleOrPart = Module
	;	current_module(Module),
		once(sub_atom(Module, _, _, _, ModuleOrPart))
	),
	module_property(Module, file(ModuleFile)),
	source_file_property(ModuleFile, load_context(LoadingModule, File:Line, _OptionList)).
%	member(if(not_loaded), OptionList),
%	member(must_be_module(true), OptionList).
	
module_imports_predicate_from(Module, Name, Arity, SuperModule, Case) :-
	has_super_module(Module, SuperModule),
	visible_in_module(Module, Name, Arity),
	
	(	defined_in_module(Module, Name, Arity)
	->	(	module_property(SuperModule, exports(ExportList)),
			member(Name/Arity, ExportList)
		->	Case = overridden
		;	Case = local
		)
	;	functor(Head, Name, Arity),
		(	predicate_property(Module:Head, imported_from(SuperModule))
		->	Case = imported
		;	Case = not_imported
		)
	).

has_super_module(Module, SuperModule) :-	
	module_property(SuperModule, file(SuperModuleFile)),
	source_file_property(SuperModuleFile, load_context(Module, _, _OptionList)).
 

loaded_file(FullPath) :-
	(	(	split_file_path(FullPath, Directory, Basename, _, lgt)
		;	split_file_path(FullPath, Directory, Basename, _, logtalk)
		) ->
		% Logtalk source file
		current_predicate(logtalk_load/1),
		% assume that Logtalk is loaded
		(	current_logtalk_flag(version, version(3, _, _)) ->
			logtalk::loaded_file(FullPath)
		;	logtalk::loaded_file(Basename, Directory)
		)
	;	% assume Prolog source file
		source_file(FullPath)
	).
