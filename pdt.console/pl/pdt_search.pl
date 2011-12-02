:- module( pdt_search,
         [ find_reference_to/12                  % (+Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind)
         , find_definitions_categorized/12       % (EnclFile,Name,Arity,ReferencedModule,Visibility, DefiningModule, File,Line)
         , find_primary_definition_visible_in/5  % (EnclFile,TermString,ReferencedModule,MainFile,FirstLine)
         , find_definition_contained_in/8
         , find_pred/8
         ]).


% TODO: The following four lines are duplicated in the loader.pl file: 
:- user:consult(pdt_runtime_builder_analyzer('meta_pred_toplevel.pl')).
:- use_module(pdt_runtime_builder_analyzer(pdt_xref_experimental)).
:- use_module(pdt_runtime_builder_analyzer(properties)).
:- use_module(pdt_prolog_library(utils4modules)).

:- use_module(split_file_path).                    % general utility

:- op(600, xfy, ::).   % Logtalk message sending operator

        /***********************************************************************
         * Find Definitions and Declarations and categorize them by visibility *
         * --------------------------------------------------------------------*
         * for "Find All Declarations" (Ctrl+G) action                         *
         ***********************************************************************/ 

% Logtalk
find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, SearchCategory, DefiningEntity, FullPath, Line, Properties, ResultsCategoryLabel):-
    Term \= _:_,
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, SearchCategory, DefiningEntity, FullPath, Line, Properties, ResultsCategory),
    results_category_label(ResultsCategory, ResultsCategoryLabel).
    
find_definitions_categorized(EnclFile,_SelectionLine,Term,Functor,Arity, ReferencedModule, definition, DefiningModule, File,Line, PropertyList, ResultsCategoryLabel):-
    search_term_to_predicate_indicator(Term, Functor/Arity),
    module_of_file(EnclFile,FileModule),
    (  atom(ReferencedModule)
    -> true                            % Explicit entity reference ReferencedModule:Term (or ReferencedModule::Term
    ;  ReferencedModule = FileModule   % Implicit module reference
    ),    
    find_decl_or_def(ReferencedModule,Functor,Arity,Sources),
    member(ResultsCategoryLabel-DefiningModule-Location,Sources),
    member(File-Lines,Location),
    member(Line,Lines),
    properties_for_predicate(ReferencedModule,Functor,Arity,PropertyList).

search_term_to_predicate_indicator(_:Term, Functor/Arity) :- !, functor(Term, Functor, Arity).
search_term_to_predicate_indicator(Term, Functor/Arity) :- functor(Term, Functor, Arity).


%% find_decl_or_def(+ContextModule,+Name,?Arity,-Visibility,-Sources)

find_decl_or_def(Module,Name,Arity,Sources) :-
    ( var(Module)
    ; var(Name)
    ),
    throw( input_argument_free(find_decl_or_def(Module,Name,Arity,Sources)) ).

find_decl_or_def(CallingModule,Name,Arity,['Missing declarations'-DeclModule-[File-[Line]]]) :-
   referenced_but_undeclared(CallingModule,Name,Arity),
   DeclModule = 'No declaration (in any module)',
   File = 'No declaration (in any file)',
   Line = 0.
   
find_decl_or_def(ContextModule,Name,Arity,Declarations) :-
   setof( VisibilityText-DeclModule-Location, ContextModule^Name^Arity^ 
          ( declared_but_undefined(DeclModule,Name,Arity),
            visibility(Visibility, ContextModule,Name,Arity,DeclModule),
            declared_in_file(DeclModule,Name,Arity,Location),
            results_context_category_label(declared, Visibility, VisibilityText)
          ),
          Declarations).
    
find_decl_or_def(ContextModule,Name,Arity,Definitions) :-
%   setof( DefiningModule, Name^Arity^
%          defined_in_module(DefiningModule,Name,Arity),
%          DefiningModules
%   ),
   setof( VisibilityText-DefiningModule-Locations, ContextModule^Name^Arity^  % Locations is list of File-Lines terms
          ( defined_in_module(DefiningModule,Name,Arity),
            visibility(Visibility, ContextModule,Name,Arity,DefiningModule),
            defined_in_files(DefiningModule,Name,Arity,Locations),
            results_context_category_label(defined, Visibility, VisibilityText)
          ),
          Definitions
    ). 

:- multifile(results_category_label/2).


:- multifile(results_context_category_label/3).

results_context_category_label(declared, local,      'Local declaration' ) :- !.
results_context_category_label(declared, supermodule,'Imported declaration' ) :- !.
results_context_category_label(declared, submodule,  'Submodule declaration') :- !.
results_context_category_label(declared, invisible,  'Locally invisible declaration') :- !.

results_context_category_label(defined, local,      'Local definitions' ) :- !.
results_context_category_label(defined, supermodule,'Imported definitions' ) :- !.
results_context_category_label(defined, submodule,  'Submodule definitions') :- !.
results_context_category_label(defined, invisible,  'Locally invisible definitions') :- !.

    
% These clauses must stay in this order since they determine
% the order of elements in the result of  find_decl_or_def
% and hence the order of elements in the search result view
% (which is the INVERSE of the order of elements that we
% compute here).               
         
visibility(invisible, ContextModule,Name,Arity,DeclModule) :-
    % Take care to generate all values befor using negation.
    % Otherwise the clause will not be able to generate values.
    % Negation DOES NOT generate values for unbound variables!
    
    % There is some DeclaringModule 
    declared_in_module(DeclModule,Name,Arity,DeclModule),
    \+ declared_in_module(ContextModule,Name,Arity,DeclModule),
    \+ declared_in_module(DeclModule,_,_,ContextModule).
    
visibility(submodule, ContextModule,Name,Arity,DeclModule) :-
    declared_in_module(DeclModule,Name,Arity,DeclModule),
    % DeclModule is a submodule of ContextModule
    declared_in_module(DeclModule,_,_,ContextModule), % submodule
    ContextModule \== DeclModule. 

visibility(supermodule, ContextModule,Name,Arity,DeclModule) :-
    declared_in_module(ContextModule,Name,Arity,DeclModule),
    ContextModule \== DeclModule. 
    
visibility(local, ContextModule,Name,Arity,DeclModule) :-
    declared_in_module(ContextModule,Name,Arity,DeclModule),
    ContextModule == DeclModule.
   

        /***********************************************************************
         * Find Primary Definition                                             *
         * --------------------------------------------------------------------*
         * for "Open Primary Declaration" (F3) action                          *
         ***********************************************************************/ 

%% find_primary_definition_visible_in(+EnclFile,+Name,+Arity,?ReferencedModule,?MainFile,?FirstLine)
%
% Find first line of first clause in the *primary* file defining the predicate Name/Arity 
% visible in ReferencedModule. In case of multifile predicates, the primary file is either 
% the file whose module is the DefiningModule or otherwise (this case only occurs
% for "magic" system modules, (e.g. 'system')) the file containing most clauses.
%
% Used for the open declaration action in 
% pdt/src/org/cs3/pdt/internal/actions/FindPredicateActionDelegate.java

        
find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine) :-
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine).


% The second argument is just an atom contianing the string representation of the term:     
find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine) :-
    atom_to_term(TermString,Term,_Bindings),
    functor(Term,Name,Arity),
    find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine).
 
% Now the second argument is a real term that is 
%  a) a file loading directive:     
find_primary_definition_visible_in__(_,Term,_,_,_,File,Line):-
    find_file(Term,File,Line).

%  b) a literal (call or clause head):    
find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine) :-
    find_definition_visible_in(EnclFile,Term,Name,Arity,ReferencedModule,DefiningModule,Locations),
    primary_location(Locations,DefiningModule,MainFile,FirstLine).


% If Term is a loading directive, find the related file,
% eventually interpreting a FileSPec that contains an alias
find_file(Term,File,Line) :-
    extract_file_spec(Term,FileSpec),
    catch( absolute_file_name(FileSpec,[solutions(all),extensions(['.pl', '.lgt', '.ct', '.ctc'])], File),
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
extract_file_spec(ensure_loaded(FileSpec),FileSpec) :- !.
extract_file_spec(Term,Term).
    
find_definition_visible_in(EnclFile,_Term,Name,Arity,ReferencedModule,DefiningModule,Locations) :-
    module_of_file(EnclFile,FileModule),
    (  atom(ReferencedModule)
    -> true                            % Explicit module reference
    ;  ReferencedModule = FileModule   % Implicit module reference
    ),
    (  defined_in_module(ReferencedModule,Name,Arity,DefiningModule)
    -> defined_in_files(DefiningModule,Name,Arity,Locations)
    ;  ( declared_in_module(ReferencedModule,Name,Arity,DeclaringModule),
    defined_in_files(DeclaringModule,Name,Arity,Locations)
       )
    ).

primary_location(Locations,DefiningModule,File,FirstLine) :-
    member(File-Lines,Locations),
    module_of_file(File,DefiningModule),
    !,
    Lines = [FirstLine|_].
primary_location(Locations,_,File,FirstLine) :-
    findall( NrOfClauses-File-FirstLine,
             ( member(File-Lines,Locations),
               length(Lines,NrOfClauses),
               Lines=[FirstLine|_]
             ),
             All
    ),
    sort(All, Sorted),
    Sorted = [ NrOfClauses-File-FirstLine |_ ].
    

        /***********************************************************************
         * Find Definitions in File                                            *
         * --------------------------------------------------------------------*
         * for Outline                                                         *
         ***********************************************************************/ 
         
% TODO: This is meanwhile subsumed by other predicates. Integrate!
   
%% find_definition_contained_in(+File, -Name,-Arity,-Line,-PropertyList) is nondet.
%
% Looks up the starting line of each clause of each  
% predicate Name/Arity defined in File. Core properties
% of the predicate are contained in the PropertyList.
%
% Called from PDTOutlineQuery.java

find_definition_contained_in(File, Entity, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList) :-
    split_file_path(File, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definition_contained_in(File, Entity, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList).

find_definition_contained_in(File, Module, module, Functor, Arity, SearchCategory, Line, PropertyList) :-
    % Backtrack over all predicates defined in File:
    source_file(ModuleHead, File),
	strip_module(ModuleHead,ModuleCandidate,Head),
	(	module_property(ModuleCandidate, file(File))
	->	Module = ModuleCandidate
	;	Module = user
	),
    functor(Head, Functor, Arity),
    properties_for_predicate(ModuleCandidate,Functor, Arity, PropertyList0),
    % In the case of a multifile predicate, we want to find all clauses for this 
    % predicate, even when they occur in other files
    (	member(multifile, PropertyList0)
    -> (	defined_in_file(ModuleCandidate, Functor, Arity, _, DeclFile, Line),
    		(	DeclFile \= File
    		-> 	(	module_property(MultiModule, file(DeclFile)),
    				append([for(MultiModule), defining_file(DeclFile)], PropertyList0, PropertyList),
    				SearchCategory = multifile
    			)
    		;	(	PropertyList = PropertyList0,
    				SearchCategory = definition
    			)
    		)
    	)
    ;	(	PropertyList = PropertyList0,
    		SearchCategory = definition,
    % The following backtracks over each clause of each predicate.
    % Do this at the end, after the things that are deterministic: 
    		(	defined_in_file(ModuleCandidate, Functor, Arity, _, File, Line)
    		->	true
    		;	defined_in_file(Module, Functor, Arity, _, File, Line)
    		)
    	)
    ).
    
% The following clause searches for clauses inside the given file, which contribute to multifile 
% predicates, defined in foreign modules.
find_definition_contained_in(File, Module, module, Functor, Arity, multifile, Line, PropertyList):-
    module_property(FileModule, file(File)),
    declared_in_module(Module,Head),
    Module \= FileModule,
    predicate_property(Module:Head, multifile),
    nth_clause(Module:Head,_,Ref),
    clause_property(Ref,file(File)),     
    clause_property(Ref,line_count(Line)),
    functor(Head, Functor, Arity),
    properties_for_predicate(Module, Functor, Arity, PropertyList0),
    append([from(Module)], PropertyList0, PropertyList).
   



               /***********************************************
                * FIND VISIBLE PREDICATE (FOR AUTOCOMPLETION) *
                ***********************************************/

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help) is nondet.
%
% Looks up all predicates with prefix Prefix defined or imported in file EnclFile.
%
% Used by the PLEditor content assist.
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
	   ( module_of_file(EnclFile,Module),
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

find_pred(_EnclFile,Prefix,EnclModule,Name,-1,true,false,'nodoc') :-
    var(EnclModule),
	current_module(Name),
    atom_concat(Prefix,_,Name).



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
     module_of_file(EnclFile,ContainingModule),
     current_predicate(ContainingModule:Name/Arity),
     functor(Head,Name,Arity),
     ( predicate_property(ContainingModule:Head,imported_from(Module))
     ; Module = ContainingModule
     ),
     !.
