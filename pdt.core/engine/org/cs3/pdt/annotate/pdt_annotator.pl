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
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(pdt_annotator,[
	pdt_ensure_annotated/1,
	pdt_op_module/2,
	%%register_annotator/1, 
	pdt_file_annotation/2,
	pdt_file_error/2,
	pdt_file_term/2,
	pdt_file_comments/2,
	pdt_file_comment/2,
	pdt_forget_annotation/1,
	pdt_annotator/2,
	pdt_annotator/3,
	pdt_annotator_enabled/2,
	pdt_set_annotator_enabled/2,
	pdt_file_record_key/3,
	pdt_file_record/2,
	pdt_bind_file_ref/2,
	trace_annotator/1
]).



:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('org/cs3/pdt/util/pdt_util_dependency')).
:- use_module(library('org/cs3/pdt/util/pdt_util_multimap')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_comments')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_preferences')).

:- use_module(library('org/cs3/pdt/annotate/cache')).

:- use_module(library('pif_observe')).

:-dynamic file_annotation/2.

:-dynamic annotator/2.

:- pdt_define_context(annotator_data(module,hooks,dependencies,enabled)).
    
:- pdt_add_preference(
	parse_comments,
	'Parse Comments', 
	'If true, the read_term/* option parse_comments will be used.',
	false
).

:- pdt_add_preference(
	default_encoding,
	'Default Encoding of Source files', 
	'The character encoding that will be used by default for all source files.',
	utf8
).


:- module_transparent pdt_maybe/1.


trace_annotator(File):-
    guitracer,
    pdt_forget_annotation(File),
    trace,
    pdt_ensure_annotated(File).

/*
pdt_maybe(+Goal)

tries to call goal, catching all exceptions.

This predicate does always succeed.
*/
pdt_maybe(Goal):-
%	catch(		
		(Goal *-> true;true).%,
%		E,
%		debugme(E)
%	).

:- module_transparent pdt_annotator/2.

%% pdt_annotator(+Hooks, +Dependencies)
%
%register an annotator hook.
%Modules that whish to register as a annotator should contain a directive
% 
% = :- pdt_annotator(+Hooks, +Dependencies) =
%
%Hooks should be a list containing the atoms =term= and/or =file=. 
%When the annotator is executed, this list will be processed element by element.
%For each 'file' element, the process_file hook will be calle for the respective annotator.
%For each 'term' element, the process_term hook will be called on each individual term for the respective
%annotator.
%
% *Update*: I added a new hook type =interleaved= which is executed interleaved with term reading.
% This makes sense for hooks that modify the read context (operators, file references, etc)
%Dependencies should be a list of file specs that contain annotator modules. The parser framework
%will make sure that all of the listed annotators are registered and get executed before the
%declaring annotator is executed.
%
%annotators can optionaly define a prediceate cleanup_hook/3 which will be called
%when the annotation of a file is about to be forgotten. These clean_up hooks will always be executed
%independently of what is listed in the hooks list above. They will be called in no particualr order,
%i.e. dependencies will not be respected.

pdt_annotator_enabled(Module,Enabled):-
    annotator(Module,Data),
    annotator_data_enabled(Data,Enabled).

pdt_set_annotator_enabled(Module,Enabled):-
    annotator(Module,Data),
    annotator_data_enabled(Data,Enabled),
    !.  
pdt_set_annotator_enabled(Module,Enabled):-
    annotator(Module,Data),
    annotator_data_set_enabled(Data,Enabled,NewData),
    retractall(annotator(Module,_)),
    assert(annotator(Module,NewData)),
    pif_notify(annotator_enabled,Module-Enabled),
    enforce_dependencies(Enabled,NewData).

enforce_dependencies(true,Data):-

	forall(annotator_data_depends(Data,Dependency),pdt_set_annotator_enabled(Dependency,true)).
enforce_dependencies(false,Data):-	
	annotator_data_module(Data,Module),
	forall(
		annotator_depends(DependingModule,Module),
		pdt_set_annotator_enabled(DependingModule,false)
	).
	
pdt_annotator(Hooks, Dependencies):-
    context_module(Module),
    pdt_annotator:pdt_annotator(Module,Hooks,Dependencies).

pdt_annotator(Module,_Hooks, _Dependencies):-
    annotator(Module,_),!.
pdt_annotator(Module,Hooks, Dependencies):-
    annotator_data_new(Data),
    annotator_data_module(Data,Module),
    annotator_data_hooks(Data,Hooks),
    annotator_data_dependencies(Data,Dependencies),
    annotator_data_enabled(Data,true),
    add_missing_hooks2(Module),
    assert(annotator(Module,Data)),
    pdt_add_node(annotator_process_order,Module),
    process_dependencies(Module,Dependencies).


process_dependencies(_Module,[]).
process_dependencies(Module,[Dependency|Dependencies]):-
    
    process_dependency(Module,Dependency),
    process_dependencies(Module,Dependencies).

process_dependency(Module,Dependency):-
%	 dependency has to be loaded
	use_module(Dependency),
	pdt_file_spec(Dependency,DependencyFile),
	current_module(DependencyModule,DependencyFile),
%    dependency has to be executed before module
	pdt_add_dependency(annotator_process_order,Module,DependencyModule).

add_missing_hooks2(Anotator):-
    add_missing_hook(Anotator:cleanup_hook/2),    
    add_missing_hook(Anotator:term_annotation_hook/5),
    add_missing_hook(Anotator:file_annotation_hook/4),
    add_missing_hook(Anotator:interleaved_annotation_hook/4).

execute_annotators(FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    pdt_process_order(annotator_process_order,Annotators),
    %format("executing annotators: ~w~n",[Annotators]),
    execute_annotators(Annotators,FileStack,OpModule,FileAnnosIn,FileAnnosOut)/*,
    format("done executing annotators~n",[])*/.

interleaved_annotators(IAs):-
    pdt_process_order(annotator_process_order,Annotators),
    filter_interleaved_annotators(Annotators,IAs).

filter_interleaved_annotators([],[]).

filter_interleaved_annotators([A|As],[A|IAs]):-
    annotator(A,Data),
    annotator_data_hooks(Data,Hooks),
    member(interleaved,Hooks),
    !,
    filter_interleaved_annotators(As,IAs).
filter_interleaved_annotators([_|As],IAs):-
    filter_interleaved_annotators(As,IAs).
    

execute_interleaved_hooks([],_,_,Term,Term).
execute_interleaved_hooks([Annotator|Annotators],FileStack,OpModule,TermIn,TermOut):-
	execute_interleaved_hook(Annotator,FileStack,OpModule,TermIn,TermNext),
	execute_interleaved_hooks(Annotators,FileStack,OpModule,TermNext,TermOut).

execute_interleaved_hook(Annotator,_FileStack,_OpModule,Term,Term):-
	annotator_disabled(Annotator),
	!.
execute_interleaved_hook(Annotator,FileStack,OpModule,TermIn,TermOut):-
    pdt_maybe(Annotator:interleaved_annotation_hook(FileStack,OpModule,TermIn,TermTmp)),
	(	var(TermTmp)
	->	TermOut=TermIn
	;	TermOut=TermTmp
	).

annotator_disabled(Annotator):-
    annotator(Annotator,Data),
    annotator_data_enabled(Data,false),
    !.
annotator_hooks(Annotator,Hooks):-
    annotator(Annotator,Data),
    annotator_data_hooks(Data,Hooks),
    !.
annotator_depends(Annotator,Dependency):-
    annotator(Annotator,Data),
    annotator_data_depends(Data,Dependency).
annotator_data_depends(Data,Dependency):-
    annotator_data_dependencies(Data,Deps),
    member(FileSpec,Deps),
    pdt_file_spec(FileSpec,File),
    current_module(Dependency,File).



execute_annotators([],_FileStack,_OpModule,FileAnnos,FileAnnos):-
	!.
execute_annotators([Annotator|Annotators],FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    execute_annotator(Annotator,FileStack,OpModule,FileAnnosIn,FileAnnosTmp),
    execute_annotators(Annotators,FileStack,OpModule,FileAnnosTmp,FileAnnosOut).

execute_annotator(Annotator,_FileStack,_OpModule,FileAnnos,FileAnnos):-
    annotator_disabled(Annotator),
    !.
execute_annotator(Annotator,FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    annotator_hooks(Annotator,Hooks),
    execute_annotator_hooks(Annotator,Hooks,FileStack,OpModule,FileAnnosIn,FileAnnosOut).

execute_annotator_hooks(_Annotator,[],_FileStack,_OpModule,FileAnnos,FileAnnos):-
	!.
execute_annotator_hooks(Annotator,[Hook|Hooks],FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    %format("executing hook ~w on ~w ...~n",[Hook,Annotator]),
    execute_annotator_hook(Annotator,Hook,FileStack,OpModule,FileAnnosIn,FileAnnosTmp),

    execute_annotator_hooks(Annotator,Hooks,FileStack,OpModule,FileAnnosTmp,FileAnnosOut).


%interleaved hooks are handled separately, see do_process_term and friends
execute_annotator_hook(_,interleaved,_,_,FileAnnos,FileAnnos):-
    !.
execute_annotator_hook(Annotator,file,FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    !,
    pdt_maybe(Annotator:file_annotation_hook(FileStack,OpModule,FileAnnosIn,TmpAnnos)),
	(	var(TmpAnnos)
	->	FileAnnosOut=FileAnnosIn
	;	FileAnnosOut=TmpAnnos
	).
execute_annotator_hook(Annotator,term,FileStack,OpModule,FileAnnos,FileAnnos):-
	FileStack=[File|_],
    file_key(term,File,Key),
    repeat,
    	recorded(Key,TermIn,Ref),
    	erase(Ref),
	    pdt_maybe(Annotator:term_annotation_hook(FileStack,OpModule,FileAnnos,TermIn,TmpTerm)),
	    (	var(TmpTerm)
	    ->	TermOut=TermIn
	    ;	TermOut=TmpTerm
	    ),
	    update_term_record(Key,TermIn,TermOut),
		TermIn==end_of_file,
	!.

% ignore annotator output at end_of_file
update_term_record(Key,end_of_file,_):-
    !,
    recordz(Key,end_of_file).
% update the record in all other cases
update_term_record(Key,_,TermOut):-    
	recordz(Key,TermOut).

%% pdt_forget_annotation(+FileSpec)
% forget all information about FileSpec.
%
% removes all data that was gathered by pdt_ensure_annotated/1.
% This includes errors, comments, terms, file annotations and index entries.
% Note however that only data associeated with the specified file is removed, i.e. this predicate
% does not recurse on the dependency graph.
pdt_forget_annotation(Spec):-
    pdt_file_spec(Spec,FileName),
%    call_cleanup_hook(FileName),
    call_cleanup_hook2(FileName),
    retractall(file_annotation(FileName,_)),
    clear_timestamp(FileName),
    clear_file_records(FileName),
    pdt_clear_cache(FileName),
    pif_notify(file_annotation(FileName),forget).



call_cleanup_hook2(FileName):-
    findall(Anotator,pdt_annotator_enabled(Anotator,true),Anotators),
    get_annos_for_cleanup(FileName,Annos),
    call_cleanup_hook2(Anotators,FileName,Annos).

get_annos_for_cleanup(FileName,Annos):-
    pdt_file_annotation(FileName,Annos),
    !.
get_annos_for_cleanup(_FileName,[]).


call_cleanup_hook2([],_,_).
call_cleanup_hook2([Annotator|Annotators],File,Annos):-
    pdt_maybe(
    	Annotator:cleanup_hook(File,Annos)
    ),
    call_cleanup_hook2(Annotators,File,Annos).     



%% pdt_file_annotation(?Filename,-FileAnotations)
% retrieve file level annotations.
%
% @param Filename should be a file spec, see pdt_util:pdt_file_spec/2.
% @param FileAnotations will be unified with a list of annotations
%   for that file.


pdt_file_annotation(FileSpec,FileAnotations):-
    nonvar(FileSpec),
    !,
    pdt_file_spec(FileSpec,Abs),    
    file_annotation(Abs,FileAnotations).
pdt_file_annotation(File,FileAnotations):-
    var(File),
    file_annotation(File,FileAnotations).    

%% pdt_file_error(?Filename,-FileAnotations)
% retrieve syntax errors in a file.
%
% @param Filename should be a file spec, see pdt_util:pdt_file_spec/2.
% @param FileAnotations will be unified with an error term.

pdt_file_error(FileSpec,Error):-
    pdt_file_record_key(error,FileSpec,Key),
    pdt_file_record(Key,Error).


%% pdt_file_comments(?Filename,-Comment)
% retrieve source comments in a file.
%
% @param Filename should be a file spec, see pdt_util:pdt_file_spec/2.
% @param Comment will be unified with a comment term as obtained by read_term/3.
pdt_file_comment(FileSpec,Comments):-
    pdt_file_record_key(comment,FileSpec,Key),
    pdt_file_record(Key,Comments).

%% pdt_file_comments(?Filename,-CommentsMap)
% retrieve comments table for a file.
%
% The comments table maps character positions to comment terms as obtained by read_term/3.
%
% @param Filename should be a file spec, see pdt_util:pdt_file_spec/2.
% @param CommentsMap will be unified with associative datastructure, see module pdt_util_map. 
pdt_file_comments(FileSpec,CommentsMap):-
    pdt_file_record_key(comment,FileSpec,Key),
        findall(Comment, pdt_file_record(Key,Comment), Comments),
    comments_map(Comments,CommentsMap).


%% pdt_file_term(?Filename,-FileAnotations)
% retrieve annotated terms in a file.
%
% this will bind the file_ref annotations to a reference to the containing file.
%
% @param Filename should be a file spec, see pdt_util:pdt_file_spec/2.
% @param FileAnotations will be unified with an annotated term. see module pdt_util_aterm.
pdt_file_term(FileSpec,Term):-
    pdt_file_record_key(term,FileSpec,Key),
	pdt_file_record(Key,Term),
	\+ (Term == end_of_file),
	pdt_bind_file_ref(FileSpec,Term).

pdt_bind_file_ref(Spec,Term):-
    (	integer(Spec)
    ->	Ref=Spec
    ;	pdt_file_ref(Spec,Ref)
    ),
	pdt_term_annotation(Term,_,Annos),
	pdt_memberchk(file_ref(Ref),Annos).



%% pdt_file_record(+Key,-Term)
% retrieve a file record.
% see pdt_file_record_key/3.
%
% @param Key a record key as retrieaved by pdt_file_record_key/3
% @param Term the term recorded for this key.
pdt_file_record(Key,Term):-
    recorded(Key,Term).


add_missing_hook(Module:Name/Arity):-
    (	Module:current_predicate(Name/Arity)
    ;	functor(Head,Name,Arity),
	    Module:assert(Head)
    ),
    !.
:- dynamic annotation_time/2.
up_to_date(File):-
    time_file(File,Time),
    annotation_time(File,Time).
update_timestamp(File):-
    time_file(File,Time),
    retractall(annotation_time(File,_)),
    assert(annotation_time(File,Time)).

clear_timestamp(File):-
    retractall(annotation_time(File,_)).



process_referenced_files([Current|Stack]):-
    pdt_file_annotation(Current,Annos),
    pdt_member(references_files(Files),Annos),
    forall( 
    	(	member(File,Files),
    		\+ member(File,[Current|Stack])
    	),	
    	pdt_ensure_annotated([File,Current|Stack])
    ).


%% pdt_ensure_annotated(+FileSpec)
% create/update annotation for a source file.
% 
% @param FileSpec should either be a single file specification or
%        a list of file specs. pdt_ensure_annotated/1 will only annotate the head entry of the list.
%      The tail is only used to pass along information on what files are
%       currently on stack, so that files that include each other do not lead
%       to infinite recursion.
% $ *Warning*: this predicate does not do the recursion check itself. If an annotator requires nested calls of 
%           this predicate, it is the responsibility of that hook to examine the stack and avoid recursions.


pdt_ensure_annotated(FileSpec):-
    \+ is_list(FileSpec),
	pdt_ensure_annotated([FileSpec]).
pdt_ensure_annotated([FileSpec|_]):-%case 1: the currrent annotation is up to date
    pdt_file_spec(FileSpec,Abs),
    up_to_date(Abs),
    %writeln(up_to_date(Abs)),
    !.
pdt_ensure_annotated([FileSpec|Stack]):-%case 2: the cached annotation is up to date
    pdt_file_spec(FileSpec,File),
	time_file(File,SourceTime),
	pdt_cache_time(File,CacheTime),
	CacheTime > 0,
	CacheTime >= SourceTime,
    %writeln(cached(File)),
    !,    
    pdt_read_cache(File),
   	update_timestamp(File),
    process_referenced_files([File|Stack]).

pdt_ensure_annotated([FileSpec|Stack]):-%case 3: cannot use cache, rebuild from scratch
	pdt_forget_annotation(FileSpec),
    new_memory_file(MemFile),
    pdt_file_spec(FileSpec,Abs),
    writeln(build(Abs)),
    gen_op_module(Abs,OpModule),
    clear_ops(OpModule),
	update_timestamp(Abs), %important to do it BEFORE the actual annotations (avoid redundant nested builds)
    pdt_preference_value(default_encoding,Enc),
    copy_file_to_memfile(FileSpec,Enc,MemFile),
    memory_file_to_atom(MemFile,MemFileAtom),
    open_memory_file(MemFile,read,Input),

    call_cleanup(
    	(    read_terms(MemFileAtom,[Abs|Stack],OpModule,Input),
		    execute_annotators([Abs|Stack],OpModule,[],FileAnnos1),	
		    assert(file_annotation(Abs,FileAnnos1))
		),	
		(	close(Input),
			free_memory_file(MemFile)
		)
	),

	%format("writing cache...~n",[]),
	%format("done~n",[]),
	pdt_write_cache(Abs),
	pif_notify(file_annotation(Abs),update),!.
pdt_ensure_annotated([FileSpec|_]):-    
    pdt_file_spec(FileSpec,Abs),
    clear_timestamp(Abs),
    fail.
    
clear_ops(OpModule):-
    forall(current_op(P,T,OpModule:N),
    		(	current_op(P,T,user:N)
    		->	true
    		;	op(0,T,OpModule:N)
    		)
    	).
	


read_terms(MemFileAtom,FileStack,OpModule,In):-
   	    interleaved_annotators(IAs),
   	    read_terms_rf(MemFileAtom,IAs,FileStack,OpModule,In,0).




read_terms_rf(MemFileAtom,IAs,FileStack,OpModule,In,0):-
    setup_options(Options),
    memberchk(module(OpModule),Options),
    flag(pdt_annotator_subterm_counter,_,0),

    repeat,
	    do_read_term(In,Term,Options,Error),
    	flag(pdt_annotator_subterm_counter,N,N),
	    do_process_term_rf(Options,MemFileAtom,IAs,FileStack,N,Term,Error,LastN),
	    (var(LastN)->debugme;true),
	    flag(pdt_annotator_subterm_counter,_,LastN+1),
	    read_terms_rf_done(Term),
	!.

debugme.

read_terms_rf_done(Term):-
    nonvar(Term),Term==end_of_file.


do_process_term_rf(_Options,_MemFileAtom,_IAs,FileStack,N,Term,_Error,N):-
	nonvar(Term),Term==end_of_file,
	record_term(FileStack,end_of_file),
	!.	
do_process_term_rf(_Options,_MemFileAtom,_IAs,FileStack,N,_Term,Error,N):-
    nonvar(Error),!,
	record_error(FileStack,Error).
do_process_term_rf(Options,MemFileAtom,IAs,FileStack,N,Term0,_Error,M):-    
	%FileStack=[File|_],
	%pdt_file_ref(File,FileRef),
	%experiment: leave file ref unbound. bind it on checkout.

	%generate variable ids
    term_variables(Term0,Vars),
    var_ids(Vars,N,FileRef,Ids,0),
    member(subterm_positions(Positions),Options),	
		
	wrap_term(Term0,Positions,FileRef,N,Term1,M),   
	pdt_term_annotation(Term1,T,A),
	memberchk(variable_names(Names),Options),
	memberchk(singletons(Singletons),Options),	

	pdt_term_annotation(ProcessedTerm0,T,[variable_ids(Ids),variable_names(Names),singletons(Singletons)|A]),
	(	memberchk(comments(TermComments),Options)
    ->	comments_map(TermComments,CommentsMap),
    	process_comments(MemFileAtom,CommentsMap,ProcessedTerm0,Options,ProcessedTerm1),
    	record_comments(FileStack,TermComments)
    ;	ProcessedTerm0=ProcessedTerm1
    ),
	memberchk(module(OpModule),Options),
	execute_interleaved_hooks(IAs,FileStack,OpModule,ProcessedTerm1,ProcessedTerm),
    record_term(FileStack,ProcessedTerm).

var_ids([],_,_,[],_).
var_ids([Var|Vars],N,F,[Var=var_id(F,N,I)|Ids],I):-
    J is I +1,

	/*FIXME: not ISO-compatible. 
	  this is currently used by the abstract unification routine
	  but could probably be done in a "clean" way.
	*/
    put_attr(Var,pdt_annotator,var_id(F,N,I)),
    
	var_ids(Vars,N,F,Ids,J).

%need this, otherwise attr vars could not be bound.
attr_unify_hook(_,_).		


record_error([File|_],Error):-
    file_key(error,File,Key),
    recordz(Key,Error).

record_comments([File|_],Comments):-
    file_key(comment,File,Key),
    record_comments_X(Key,Comments).

record_comments_X(_Key,[]).
record_comments_X(Key,[Comment|Comments]):-
    recordz(Key,Comment),
    record_comments_X(Key,Comments).

record_term([File|_],Term):-
    file_key(term,File,Key),
    recordz(Key,Term).

clear_file_records(File):-
	forall((file_key(_,File,Key),recorded(Key,_,Ref)),erase(Ref)).


%% pdt_file_record_key(+Topic,+FileSpec,-Key)
% retrieve a record key for the given File and topic.
%
% The annotator stores different kinds of information on processed files
% in the record database. The record keys are unique for each (file, topic) pair.
% The record keys optained from this predicate can be used together with
% pdt_file_record/2 to access the recorded data.
%
% Currently, the following topics are supported:
% $=term=: records all annotated terms. See also pdt_file_term/2
% $=comment=: records lists of comments as obtained via read_term/3
% $=errors=:records syntax errors.
%
% @param Topic the topic
% @param FileSpec a file specification. See pdt_util:pdt_file_spec/2.
% @param Key will be unified with the record key.
pdt_file_record_key(Kind,FileRef,Key):-
    file_key(Kind,FileRef,Key).

file_key(Kind,file_ref(Ref),Key):-
    !,
    do_file_key(Kind,Ref,Key).
file_key(Kind,FileSpec,Key):-
    pdt_file_ref(FileSpec,Ref),
    do_file_key(Kind,Ref,Key).
    
do_file_key(term,Ref,Key):-
    concat_atom([pdt_annotator,term,Ref],'$',Key).
do_file_key(error,Ref,Key):-
    concat_atom([pdt_annotator,error,Ref],'$',Key).
do_file_key(comment,Ref,Key):-
    concat_atom([pdt_annotator,comment,Ref],'$',Key).



setup_options(Options):-
    pdt_preference_value(parse_comments,true),
    !,
    Options=[
		subterm_positions(_),
		variable_names(_),
		singletons(_),
    		module(_Module),
    		comments(_TermComments)
    	].

setup_options(Options):-
    Options=[
		subterm_positions(_),
		variable_names(_),
		singletons(_),
    		module(_Module),
    		double_quotes(string)
    	].

    	

do_read_term(In,Term,Options,Error):-
    catch(
		read_term(In,Term,Options),
		Error,
		true 
	).

process_comments(MemFileAtom,CommentsMap,ATermIn,Options,ATermOut):-
    memberchk(comments(Comments),Options),
    !,
	comment_positions(Comments,CommentPositions),
	atom_to_memory_file(MemFileAtom,MemFile),
	open_memory_file(MemFile,read,Stream),
	call_cleanup(
		pdt_attach_comments(ATermIn,CommentsMap,Stream,CommentPositions,_,ATermOut),
		close(Stream)
	),
	free_memory_file(MemFile).
process_comments(_MemFileAtom,_CommentsMap,ATerm,_Options,ATerm).

% comment_positions(+Comments, -Positions)
%
% extract the character offsets from a list of comments as returned by read_term/*
comment_positions([],[]).
comment_positions([CPos-_|Cs],[Position|Positions]):-
    stream_position_data(char_count,CPos,Position),
    comment_positions(Cs,Positions).


% comments_map(+Comments,-Map)
%
% create a associative datastructure that maps comment positions to comment strings
% Comments is a list of comments as returned by read_term
% Map is a associative map. (See pdt_util_map) 
% The map contains character offsets of comments as keys and comment strings as values.
comments_map(Comments,Map):-
    pdt_map_empty(Map0),
    comments_map(Map0,Comments,Map).

% comments_map(+MapIn, +Comments,-MapOut)
%
% add position --> comment mappings to an existing map.
% Comments is a list of comments as returned by read_term
% Map is a associative map. (See pdt_util_map) 
% The map contains character offsets of comments as keys and comment strings as values.    
comments_map(In,[],In).
comments_map(In,[CPos-Comment|Cs],Out):-
    stream_position_data(char_count,CPos,Position),
    pdt_map_put(In,Position,CPos-Comment,Next),
    comments_map(Next,Cs,Out).


		
%% pdt_op_module(+FileSpec,-OpModule)
% access the operator module for a source file.
%
% The annotator uses a separate module for each file to cope with operator redefinitions and the like.
% ( This will hopefully be obsoleted soon by the prolog_source library.)

pdt_op_module(FileSpec,OpModule):-
	pdt_file_spec(FileSpec,Abs),
	gen_op_module(Abs,OpModule).
    
gen_op_module(Abs,OpModule):-
    concat_atom([Abs,'$',op_module],OpModule).
    
	    
	
	
	