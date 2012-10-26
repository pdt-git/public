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
         [ find_reference_to/12                  % (+Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind)
         , find_definitions_categorized/13       % (+EnclFile,+SelectionLine, +Term, -Functor, -Arity, -This, -DeclOrDef, -DefiningEntity, -FullPath, -Line, -Properties,-Visibility,+ExactMatch)
         , find_definitions_categorized/9
         , find_primary_definition_visible_in/7  % (EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult)
         , find_definition_contained_in/9
         , find_pred/8
         , find_pred_for_editor_completion/9
         , find_entity_definition/5
         , find_module_reference/8
         , find_alternative_predicates/7
         ]).

:- use_module( prolog_connector_pl(split_file_path),
             [ split_file_path/4                % (File,Folder,FileName,BaseName,Extension)
             ] ).
:- reexport(   'xref/pdt_xref', 
             [ find_reference_to/12             % ...
             ] ).
:- use_module( properties, 
             [ properties_for_predicate/4
             ] ).
:- use_module( pdt_prolog_library(utils4modules),
             [ module_of_file/2                 % (File,FileModule)
             , defined_in/4             % (SubModule,Name,Arity,DeclModule),
             , defined_in_module/3              % (Module,Name,Arity),
             , declared_in_file/4               % (Module,Name,Arity,Location)
             , defined_in_files/4               % (Module,Name,Arity,Locations)
             ] ).
:- use_module(pdt_prolog_library(utils4modules_visibility)).

% TODO: Why this import?
:- user:consult(pdt_builder_analyzer('meta_pred_toplevel.pl')).

:- use_module(library(charsio)). 
:- use_module(library(lists)). 

:- op(600, xfy, ::).   % Logtalk message sending operator

        /***********************************************************************
         * Find Definitions and Declarations and categorize them by visibility *
         * --------------------------------------------------------------------*
         * for "Find All Declarations" (Ctrl+G) action                         *
         ***********************************************************************/ 

%% find_definitions_categorized(+Term,+ ExactMatch, -DefiningModule, -Functor, -Arity, -DeclOrDef, -FullPath, -Line, -Properties) is nondet.
% 
find_definitions_categorized(Term, ExactMatch, DefiningModule, Functor, Arity, DeclOrDef, File,Line, PropertyList):-
	(	atom(Term)
	->	SearchFunctor = Term
	;	Term = SearchFunctor/Arity
	),
	(	ExactMatch == true
	->	Functor = SearchFunctor,
		declared_in_module(DefiningModule, Functor, Arity, DefiningModule)
	;	declared_in_module(DefiningModule, Functor, Arity, DefiningModule),
		once(sub_atom(Functor,_,_,_,SearchFunctor))
	),
    find_decl_or_def_2(Functor,Arity,Sources),              % Unique, grouped sources (--> setof)
    member(DeclOrDef-DefiningModule-Location,Sources),
    member(File-Lines,Location),
    member(location(Line, Ref),Lines),
    properties_for_predicate(DefiningModule,Functor,Arity,PropertyList0),
    (	first_argument_of_clause(Ref, FirstArgument)
    ->	PropertyList = [FirstArgument|PropertyList0]
    ;	PropertyList = PropertyList0
    ).

find_definitions_categorized(Term, ExactMatch, DefiningModule, Functor, Arity, DeclOrDef, File,Line, PropertyList) :-
	current_predicate(logtalk_load/1),
	logtalk_adapter::find_definitions_categorized(Term, ExactMatch, DefiningModule, Functor, Arity, DeclOrDef, File,Line, PropertyList).


% find_definitions_categorized(+ReferencingFile,+-ReferencingLine,+ReferencingTerm,-Name,-Arity, 
%                               ???ReferencingModule, -DefiningModule, -DeclOrDef, -Visibility, -File,-Line)
%                                                      ^^^^^^^^^^^^^^ TODO: moved to this place (two arguments forward)
% Logtalk
find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility, _ExactMatch):-
    Term \= _:_,
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility).
    
    
find_definitions_categorized(EnclFile,_SelectionLine,Term,Functor,Arity, ReferencedModule, DeclOrDef, DefiningModule, File,Line, PropertyList, Visibility, _ExactMatch):-
    referenced_entity(EnclFile, ReferencedModule),    
    search_term_to_predicate_indicator(Term, Functor/Arity),
    find_decl_or_def(ReferencedModule,Functor,Arity,Sources),              % Unique, grouped sources (--> setof)
    member(DeclOrDef-Visibility-DefiningModule-Location,Sources),
    member(File-Lines,Location),
    member(location(Line, Ref),Lines),
    properties_for_predicate(DefiningModule,Functor,Arity,PropertyList0),
    (	first_argument_of_clause(Ref, FirstArgument)
    ->	PropertyList = [FirstArgument|PropertyList0]
    ;	PropertyList = PropertyList0
    ).


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
    logtalk_adapter::find_primary_definition_visible_in(EnclFile, SelectedLine,Term,_Functor,_Arity,ReferencedModule,MainFile,FirstLine).

% The second argument is just an atom contianing the string representation of the term:     
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

        /***********************************************************************
         * Find Definitions in File                                            *
         * --------------------------------------------------------------------*
         * for Outline                                                         *
         ***********************************************************************/ 
         
% TODO: This is meanwhile subsumed by other predicates. Integrate!
   
%% find_definition_contained_in(+File, -Entity, -EntityLine, -EntityKind, -Name, -Arity, -SearchCategory, -Line, -PropertyList) is nondet.
%
% Look up the starting line of a clause of a predicate Name/Arity defined in File. 
% Do this upon backtracking for each clause of each predicate in File.
%
% @param File           The file to search for definitions.
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
    split_file_path(File, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definition_contained_in(File, Entity, EntityLine, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList).

find_definition_contained_in(ContextFile, DefiningModule, ModuleLine, module, Functor, Arity, SearchCategory, Line, PropertyList) :-
    SearchCategory = definition,
    
%    module_of_file(ContextFile, ContextModule),
    % Backtrack over all predicates defined in File
    % including multifile contributions to other modules:
    source_file(ModuleHead, ContextFile),
    pdt_strip_module(ModuleHead,DefiningModule,ModuleLine,Head),
    
    % Predicate properties:
    functor(Head, Functor, Arity),
    \+ find_blacklist(Functor,Arity,DefiningModule),
    properties_for_predicate(DefiningModule,Functor, Arity, PropertyList0),

    % In the case of a multifile predicate, we want to find all clauses for this 
    % predicate, even when they occur in other files
    defined_in_file(DefiningModule, Functor, Arity, Ref, _, DefiningFile, Line),
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

first_argument_of_clause(Ref, first_argument(Arg)) :-
	catch(clause(_:Head, _, Ref), _, fail),
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

% pdt_strip_module(+ModuleHead,?Module,-ModuleLine,?Head) is det
% pdt_strip_module(-ModuleHead,?Module,-ModuleLine,+Head) is det
%
% If a predicate is defined in a proper module, 
% ModuleLine is the line of the module declaration.
% Otherwise, it is implicitly defined in "user" at line 1.
%
% Unlike the strip_module/3 of SWI-Prolog it does not add the module
% in which it is called if the ModuleHead has no module prefix.
pdt_strip_module(Module:Head,Module,Line,Head) :- 
     atom(Module),                               % If we are in a proper module
     !,
     module_property(Module, line_count(Line)).  % ... get its line number
pdt_strip_module(Head,user,1,Head).              
	
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

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help) is nondet.
%
% Looks up all predicates with prefix Prefix defined or imported in file EnclFile.
%
% Used by the PLEditor content assist.
%
% The meaning of Arity is overloaded: -2: atom, -1 : module, >= 0 : predicate
% 
% For performance reasons an empty prefix with an unspecified module
% will only bind predicates if EnclFile is specified.
%
% <EnclFile> specifies the file in which this query is triggered
% <Prefix> specifies the prefix of the predicate
% <Module> specifies the module associated to the file.

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help).


find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
    \+ atom(EnclFile),
    throw( first_argument_free_in_call_to(find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help))).

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
	setof(
	   (Name,Arity),
	   Prefix^Module^
	   ( my_module_of_file(EnclFile,Module),
	     find_pred_(Prefix,Module,Name,Arity,true)
	   ),
	   All
	),
	member((Name,Arity),All),
	
	% no enclosing module specified in the code via modulename:..
	get_declaring_module(EnclFile,Module,Name,Arity),
	functor(Term,Name,Arity),
	( predicate_property(Module:Term,exported)->
	  Exported=true
	; Exported=false
	),
	( predicate_property(Module:Term,built_in)->
	  Builtin=true
	; Builtin=false
	),
	predicate_manual_entry(Module,Name,Arity,Help).

find_pred_(Prefix,Module,Name,Arity,true) :-
    ( var(Module)->
    	Prefix \== ''
    ; true
    ), % performance tweak:
    current_predicate(Module:Name/Arity),
    atom_concat(Prefix,_,Name),
    % rule out used built-ins, like =../2, in case the enclosing module is given (in this case the prefix might be empty):   
    ( nonvar(Module) ->
      ( functor(Term,Name,Arity),
    	(Prefix \== ''; \+ predicate_property(Term, built_in)) )
      ; true
    ).


get_declaring_module(EnclFile,Module,Name,Arity) :-
	var(Module),
     my_module_of_file(EnclFile,ContainingModule),
     current_predicate(ContainingModule:Name/Arity),
     functor(Head,Name,Arity),
     ( predicate_property(ContainingModule:Head,imported_from(Module))
     ; Module = ContainingModule
     ),
     !.

get_declaring_module(_EnclFile,Module,_Name,_Arity) :-
	nonvar(Module),
	!.

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help, -Kind) is nondet.
%

find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,Kind) :-
    \+ atom(EnclFile),
    throw( first_argument_free_in_call_to(find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,Kind))).

find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,predicate) :-
	find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help).

find_pred_for_editor_completion(_EnclFile,Prefix,EnclModule,Name,-1,true,false,'nodoc', module) :-
    var(EnclModule),
	current_module(Name),
    atom_concat(Prefix,_,Name).

% TODO: Improvement Idea: use "string" Prefix instead 
%  of atom to avoid Prefix to be added to the set of atoms
find_pred_for_editor_completion(_EnclFile,Prefix,'',Atom,-1,fail,true,'nodoc', atom) :-
	'$atom_completions'(Prefix, Atoms),
	member(Atom,Atoms), 
	Atom \= Prefix,
	garbage_collect_atoms,
	\+ current_predicate(Atom/_Arity).

my_module_of_file(_File, Module) :-
	atom(Module),
	current_module(Module),
	!.

my_module_of_file(File,Module):-
    module_property(Module2,file(File)),
	(	Module = Module2
	;	Module = user
	).
                                       
my_module_of_file(File,Module):-
    atom(File),                           
    \+ module_property(Module,file(File)),
    ( Module=user                         
    ; Module=system                       
    ).


find_entity_definition(SearchString, ExactMatch, File, Line, Entity) :-
	current_predicate(logtalk_load/1),
	!,
	logtalk_adapter::find_entity_definition(SearchString, ExactMatch, File, Line, Entity).

find_entity_definition(SearchString, ExactMatch, File, Line, Entity) :-
	find_module_definition(SearchString, ExactMatch, File, Line, Entity).
	

find_module_definition(SearchModule, ExactMatch, File, Line, Module) :-
	current_module(Module),
	(	ExactMatch == true
	->	SearchModule = Module
	;	once(sub_atom(Module, _, _, _, SearchModule))
	),
	module_property(Module, file(File)),
	module_property(Module, line_count(Line)).
	

find_module_reference(Module, ExactMatch, File, Line, system, load_files, 2, PropertyList) :-
	once(properties_for_predicate(system, load_files, 2, PropertyList)),
	find_use_module(Module, ExactMatch, _, _, File, Line). 

find_module_reference(Module, ExactMatch, File, Line, ReferencingModule, RefName, RefArity, PropertyList) :-
	search_module_name(Module, ExactMatch, SearchModule),
	find_reference_to(_, _, _, SearchModule, ExactMatch, ReferencingModule, RefName, RefArity, File, Line, _, _, PropertyList).

search_module_name(Module, true, Module) :- !.
search_module_name(ModulePart, false, Module) :-
	current_module(Module),
	once(sub_atom(Module, _, _, _, ModulePart)).

find_use_module(ModuleOrPart, ExactMatch, ModuleFile, LoadingModule, File, Line) :-
	(	ExactMatch == true
	->	ModuleOrPart = Module
	;	current_module(Module),
		once(sub_atom(Module, _, _, _, ModuleOrPart))
	),
	module_property(Module, file(ModuleFile)),
	source_file_property(ModuleFile, load_context(LoadingModule, File:Line, OptionList)),
%	member(if(not_loaded), OptionList),
	member(must_be_module(true), OptionList).
	
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
 
