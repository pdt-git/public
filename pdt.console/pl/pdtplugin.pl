%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
%
% Author: Lukas Degener (among others)
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
%
% All rights reserved. This program is  made available under the terms
% of the Eclipse Public License v1.0 which accompanies this distribution,
% and is available at http://www.eclipse.org/legal/epl-v10.html
%
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
%
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%
% ad 1: A program part is said to 'depend, either directly or indirectly,
%   on parts of the Eclipse framework', if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the 'pdt' package
%   fragment in their name fall into this category.
%
% ad 2: 'Third-party code' means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* The module pdtplugin provides helper predicates for the PDT Eclipse Plugin */

:- module(pdtplugin,[
    pdt_reload/1,  
    find_reference_to/11, % +Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind
    find_definitions_categorized/8, % (EnclFile,Name,Arity,ReferencedModule,Visibility, DefiningModule, File,Line):-
    find_primary_definition_visible_in/7, % (EnclFile,TermString,Name,Arity,ReferencedModule,MainFile,FirstLine)
    get_pred/7,
    find_pred/8,
    predicates_with_property/3,
    manual_entry/3, % still in use, but probably broken, see predicat_manual_entry
    activate_warning_and_error_tracing/0,
    deactivate_warning_and_error_tracing/0,
    errors_and_warnings/4
    ]).

:- use_module(library(pldoc/doc_library)).
:- use_module(library(explain)).
:- use_module(library(help)).
:- use_module(library(make)).
:- use_module(library('pldoc')).
:- use_module(library('pldoc/doc_html')).
:- use_module(library('http/html_write')).

:- use_module(pdt_runtime_builder_analyzer('metafile_referencer.pl')).

:- consult(org_cs3_lp_utils(general)).
:- use_module(org_cs3_lp_utils(utils4modules)).
:- use_module(org_cs3_lp_utils(pdt_xref_experimental)).

             
               /**********************************
                * TODOs for EVA after WLPE paper *
                * ********************************/
 

% Move the definition of has_property/3 to the "share" project. 
% It is currently defined in JT making the PDT dependent on JT!!! 
% -- GK, 21,4,2011   

% TO BE INLINED into call site in
% pdt/src/org/cs3/pdt/internal/actions/PrologOutlineInformationControl.java
get_pred(File, Name,Arity,Line,Dynamic,Multifile,Exported) :-
    find_definition_contained_in(File,Name,Arity,Line,Dynamic,Multifile,Exported).

               /*************************************
                * PDT RELAOAD                       *
                *************************************/

%% pdt_reload(File) is det.
%
% wrapper for consult. Only used to ignore PLEditor triggered consults in the history.
%
pdt_reload(File):-
    writeln(File),
    make:reload_file(File).  % SWI Prolog library



        /***********************************************************************
         * Find Definitions and Declarations and categorize them by visibility *
         * --------------------------------------------------------------------*
         * for "Find All Declarations" (Ctrl+G) action                         *
         ***********************************************************************/ 

find_definitions_categorized(EnclFile,Name,Arity,ReferencedModule,Category, DefiningModule, File,Line):-
    module_of_file(EnclFile,FileModule),
    (  atom(ReferencedModule)
    -> true                            % Explicit module reference
    ;  ReferencedModule = FileModule   % Implicit module reference
    ),    
    find_decl_or_def(ReferencedModule,Name,Arity,Sources),
    member(Category-DefiningModule-Location,Sources),
    member(File-Lines,Location),
    member(Line,Lines).

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
            visibility_text(declared, Visibility, VisibilityText)
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
            visibility_text(defined, Visibility, VisibilityText)
          ),
          Definitions
    ). 


visibility_text(declared, local,      'Local declaration' ) :- !.
visibility_text(declared, supermodule,'Imported declaration' ) :- !.
visibility_text(declared, submodule,  'Submodule declaration') :- !.
visibility_text(declared, invisible,  'Locally invisible declaration') :- !.

visibility_text(defined, local,      'Local definitions' ) :- !.
visibility_text(defined, supermodule,'Imported definitions' ) :- !.
visibility_text(defined, submodule,  'Submodule definitions') :- !.
visibility_text(defined, invisible,  'Locally invisible definitions') :- !.

    
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

% The second argument is just an atom contianing the string representation of the term:     
find_primary_definition_visible_in(EnclFile,TermString,Name,Arity,ReferencedModule,MainFile,FirstLine):-
    atom_to_term(TermString,Term,_Bindings),
    find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine).
 
% Now the second argument is a real term:     
find_primary_definition_visible_in__(_,Term,_,_,_,File,Line):-
    extract_file_spec(Term,FileSpec),
    absolute_file_name(FileSpec,[solutions(all),extensions(['.pl', '.lgt', '.ct', '.ctc'])], File),
    access_file(File, read),
    !,
    Line=1.
    
find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine):-
    find_definition_visible_in(EnclFile,Term,Name,Arity,ReferencedModule,DefiningModule,Locations),
    primary_location(Locations,DefiningModule,MainFile,FirstLine).


% Work regardelessly whether the user selected the entire consult/use_module
% statement or just the file spec. Does NOT work if he only selected a file
% name within an alias but not the complete alias.
extract_file_spec(consult(FileSpec),FileSpec) :- !.
extract_file_spec(use_module(FileSpec),FileSpec) :- !.
extract_file_spec(Term,Term).
    
find_definition_visible_in(EnclFile,Term,Name,Arity,ReferencedModule,DefiningModule,Locations) :-
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
         * Find Primary Definition                                             *
         * --------------------------------------------------------------------*
         * for "Open Primary Declaration" (F3) action                          *
         ***********************************************************************/ 
         
% TODO: This is meanwhile subsumed by other predicates. Integrate!
   
%% find_definition_contained_in(+File, -Name,-Arity,-Line,-Dyn,-Mul,-Exported) is nondet.
%
% Looks up the starting line of each clause of each  
% predicate Name/Arity defined in File. The boolean
% properties Dyn(amic), Mul(tifile) and Exported are
% unified with 1 or 0.
%
% Called from PrologOutlineInformationControl.java

find_definition_contained_in(File, Name,Arity,Line,Dyn,Mul,Exported) :-
    % Backtrack over all predicates defined in File:
    source_file(ModuleHead, File),
	strip_module(ModuleHead,Module,Head),
    functor(Head,Name,Arity),
    has_property(Head,dynamic,Dyn),
    has_property(Head,multifile,Mul),
    has_property(Head,exported,Exported),
    % The following backtracks over each clause of each predicate.
    % Do this at the end, after the things that are deterministic: 
    defined_in_file(Module,Name,Arity,_Nth,File,Line).


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

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help):-
    \+ atom(EnclFile),
    throw( first_argument_free_in_call_to(find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help))).

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help):-
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

find_pred(_EnclFile,Prefix,EnclModule,Name,-1,true,false,'nodoc'):-
    var(EnclModule),
	current_module(Name),
    atom_concat(Prefix,_,Name).



find_pred_(Prefix,Module,Name,Arity,true):-
    ( var(Module)->
    	not(Prefix == '')
    ; true
    ), % performance tweak:
    current_predicate(Module:Name/Arity),
    atom_concat(Prefix,_,Name),
    % rule out used built-ins, like =../2, in case the enclosing module is given (in this case the prefix might be empty):   
    ( nonvar(Module) ->
      ( functor(Term,Name,Arity),
    	(not(Prefix == '');not(built_in(Term))) )
      ; true
    ).

get_declaring_module(EnclFile,Module,Name,Arity):-
     module_of_file(EnclFile,ContainingModule),
     current_predicate(ContainingModule:Name/Arity),
     functor(Head,Name,Arity),
     ( predicate_property(ContainingModule:Head,imported_from(Module))
     ; Module = ContainingModule
     ),
     !.


               /****************************************
                * GET THE MANUAL ENTRY FOR A PREDICATE *
                ****************************************/

%% predicate_manual_entry(+Module, +Pred,+Arity,-Content) is det.
%
%
predicate_manual_entry(_Module,Pred,Arity,Content) :-
    help_index:predicate(Pred,Arity,_,FromLine,ToLine),
    !,
    online_help:line_start(FromLine, From),
    online_help:line_start(ToLine, To),
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    seek(Manual,From,bof,_NewOffset),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle), 
    !.


predicate_manual_entry(Module, Pred,Arity,Content) :-
    %pldoc:doc_comment(Module:Pred/Arity,_File:_,,Content),
    %TODO: The html code is now available:
	pldoc:doc_comment(Module:Pred/Arity,File:_,_Summary,_Content),
	gen_html_for_pred_(File,Pred/Arity,Content),
    !.
	
predicate_manual_entry(_Module,_Pred,_Arity,'nodoc').

gen_html_for_pred_(FileSpec,Functor/Arity,Html):-    
	doc_file_objects(FileSpec, _File, Objects, FileOptions, []),
	member(doc(Signature,FilePos,Doc),Objects),
	(Functor/Arity=Signature;_Module:Functor/Arity=Signature),
	phrase(html([ 
	     		\objects([doc(Functor/Arity,FilePos,Doc)], FileOptions)
	     ]),List),
	maplist(replace_nl_,List,AtomList),
	concat_atom(AtomList,Html), 
	!.

replace_nl_(nl(_),''):-!. 
replace_nl_(A,A).

write_ranges_to_file(Ranges, Outfile) :-
    online_manual_stream(Manual),
    help_tmp_file(Outfile),
    open(Outfile, write, Output),
    show_ranges(Ranges, Manual, Output),
    close(Manual),
    close(Output).

%% manual_entry(Pred,Arity,Content) is det.
%
% TODO: take over code, or reference predicate_manual_entry
%
manual_entry(Pred,Arity,Content) :-
    predicate(Pred,Arity,_,From,To),
    !,
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    stream_position(Manual, _, '$stream_position'(From, 0, 0)),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle).
/*
manual_entry(Pred,Arity,Content) :-
    meta_data_help(_,Pred,Arity,ContentString),
    string_to_atom(ContentString,Content).

manual_entry(Pred,-1,Content) :-
    meta_data_module(_,Pred,ContentString),
    string_to_atom(ContentString,Content).
*/




               /*************************************
                * PROLOG ERROR MESSAGE HOOK         *
                *************************************/

:- dynamic traced_messages/3.
:- dynamic warning_and_error_tracing/0.

activate_warning_and_error_tracing :- 
	assert(warning_and_error_tracing).

deactivate_warning_and_error_tracing:-
	retractall(warning_and_error_tracing),
	retractall(traced_messages(_,_,_)).
 
 
%% message_hook(+Term, +Level,+ Lines) is det. 
%
% intercept prolog messages to collect term positions and 
% error/warning messages in traced_messages/3
% 
% @author trho
%  
user:message_hook(_Term, Level,Lines) :-
  warning_and_error_tracing,
  prolog_load_context(term_position, '$stream_position'(_,Line,_,_,_)),
  assert(traced_messages(Level, Line,Lines)),
  fail.

%% errors_and_warnings(Level,Line,Length,Message) is nondet.
%
errors_and_warnings(Level,Line,0,Message):-
    traced_messages(Level, Line,Lines),
%	traced_messages(error(syntax_error(_Message), file(_File, StartLine, Length, _)), Level,Lines),
    new_memory_file(Handle),
   	open_memory_file(Handle, write, Stream),
	print_message_lines(Stream,'',Lines),
    close(Stream),
	memory_file_to_atom(Handle,Message),
    free_memory_file(Handle).
      


               /*****************************************
                * PREDICATE PROPERTIES FOR HIGHLIGHTING *
                *****************************************/
                

%% predicates_with_property(+Property,-Predicates) is det.
%
% Look up all Predicates with property Property, including atomic
% properties (e.g. dynamic, built_in) AND properties that are 
% functions (e.g. meta_predicate(Head)).

% GK, 5. April 2011: Extended the implementation to deal with unary
% functors. The combination of findall and setof is essentail for 
% this added functionality. The findall/3 call finds all results
%   (even if the arguments are free variables -- note that setof/3
%   would return results one by one in such a case, not a full list!). 
% Then the setof/3 call eliminates the duplicates from the results
% of findall/3. 
% DO NOT CHANGE, unless you consider yourself a Prolog expert.

% Property = undefined | built_in | dynamic | transparent | meta_predicate(_)    

% Look for undefined predicates only in the local context 
% (of the file whose editor has just been opened):
%predicates_with_property(undefined, FileName, Predicates) :-
%    !,
%    module_of_file(FileName,Module), 
%	findall(Name, predicate_name_with_property_(Module,Name,undefined), AllPredicateNames),
%	make_duplicate_free_string(AllPredicateNames,Predicates).

predicates_with_property(Property, _, Predicates) :-
    findall(Name, predicate_name_with_property_(_,Name,Property), AllPredicateNames),
	make_duplicate_free_string(AllPredicateNames,Predicates).


    	
predicate_name_with_property_(Module,Name,Property):-
    current_module(Module),
    current_predicate(Module:Name/Arity),
	Name \= '[]',
	functor(Head,Name,Arity),
	predicate_property(Module:Head,Property).
	
make_duplicate_free_string(AllPredicateNames,Predicates) :-
    setof(Name, member(Name,AllPredicateNames), UniqueNames),
	sformat(S,'~w',[UniqueNames]),
	string_to_atom(S,Predicates).


	
%% predicates_with_unary_property(+Property,?Predicates,?PropertyParams) is det.
%
% Look up all Predicates with the unary property Property, e.g. meta_predicate(Head) 
% The element at position i in Predicates is the name of a predicate that has  
% the property Property with the parameter at position i in PropertyParams.
%
% Author: GK, 5 April 2011
% TODO: Integrate into the editor the ability to show the params as tool tips,
% e.g. show the metaargument specifications of a metapredicate on mouse over.
predicates_with_unary_property(Property,Predicates,PropertyArguments):-
	setof((Name,Arg),
	   predicate_name_with_unary_property_(Name,Property,Arg),
	   PredArgList),
	findall(Pred, member((Pred,_),PredArgList), AllProps),
	findall(Arg,  member((_,Arg), PredArgList), AllArgs),
	sformat(S1,'~w',[AllProps]),
	sformat(S2,'~w',[AllArgs]),
	string_to_atom(S1,Predicates),
	string_to_atom(S2,PropertyArguments).
	   	  
% helper
predicate_name_with_unary_property_(Name,Property,Arg):-
    Property =.. [__F,Arg],
	predicate_property(_M:Head,Property),
	functor(Head,Name,_),
	Name \= '[]'.
	


	