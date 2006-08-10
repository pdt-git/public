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
	ensure_annotated/1,
	get_op_module/2,
	%%register_annotator/1, 
	current_file_annotation/2,
	current_file_error/2,
	current_file_term/2,
	current_file_comments/2,
	forget_file_annotation/1,
	pdt_annotator/2,
	pdt_annotator/3,
	pdt_file_record_key/3,
	pdt_file_record/2
]).



:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('org/cs3/pdt/util/pdt_util_hashtable')).
:- use_module(library('org/cs3/pdt/util/pdt_util_dependency')).
:- use_module(library('org/cs3/pdt/util/pdt_util_multimap')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_comments')).
:- use_module(library('org/cs3/pdt/util/pdt_preferences')).
:- use_module(library('pif_observe')).

:-dynamic file_annotation/2.
:-dynamic file_comments/2.
:-dynamic file_error/2.
:-dynamic annotator/2.
:-dynamic annotator/3.


:- pdt_add_preference(
	parse_comments,
	'Parse Comments', 
	'If true, the read_term/* option parse_comments will be used.',
	false
).

:- module_transparent pdt_maybe/1.
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
/**
pdt_annotator(+Hooks, +Dependencies)

New hook registration api.
Modules that whish to register as a annotator should contain a directive
:- pdt_annotator(+Hooks, +Dependencies)
Hooks should be a list containing the atoms 'term' and/or 'file'. 
When the annotator is executed, this list will be processed element by element.
For each 'file' element, the process_file hook will be calle for the respective annotator.
For each 'term' element, the process_term hook will be called on each individual term for the respective
annotator.

Dependencies should be a list of file specs that contain annotator modules. The parser framework
will make sure that all of the listed annotators are registered and get executed before the
declaring annotator is executed.

annotators can optionaly define a prediceate cleanup_hook/2 which will be called
when the annotation of a file is about to be forgotten. These clean_up hooks will always be executed
independently of what is listed in the hooks list above. They will be called in no particualr order,
i.e. dependencies will not be respected.

*/
:- module_transparent pdt_annotator/2.
pdt_annotator(Hooks, Dependencies):-
    context_module(Module),
    pdt_annotator:pdt_annotator(Module,Hooks,Dependencies).

pdt_annotator(Module,_Hooks, _Dependencies):-
    annotator(Module,_,_),!.
pdt_annotator(Module,Hooks, Dependencies):-
    add_missing_hooks2(Module),
    assert(annotator(Module,Hooks,Dependencies)),
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
    execute_annotators(Annotators,FileStack,OpModule,FileAnnosIn,FileAnnosOut).

interleaved_annotators(IAs):-
    pdt_process_order(annotator_process_order,Annotators),
    filter_interleaved_annotators(Annotators,IAs).

filter_interleaved_annotators([],[]).

filter_interleaved_annotators([A|As],[A|IAs]):-
    annotator(A,Hooks,_),
    member(interleaved,Hooks),
    !,
    filter_interleaved_annotators(As,IAs).
filter_interleaved_annotators([_|As],IAs):-
    filter_interleaved_annotators(As,IAs).
    

execute_interleaved_hooks([],_,_,Term,Term).
execute_interleaved_hooks([Annotator|Annotators],FileStack,OpModule,TermIn,TermOut):-
	execute_interleaved_hook(Annotator,FileStack,OpModule,TermIn,TermNext),
	execute_interleaved_hooks(Annotators,FileStack,OpModule,TermNext,TermOut).
	
execute_interleaved_hook(Annotator,FileStack,OpModule,TermIn,TermOut):-
    pdt_maybe(Annotator:interleaved_annotation_hook(FileStack,OpModule,TermIn,TermTmp)),
	(	var(TermTmp)
	->	TermOut=TermIn
	;	TermOut=TermTmp
	).


execute_annotators([],_FileStack,_OpModule,FileAnnos,FileAnnos).
execute_annotators([Annotator|Annotators],FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    execute_annotator(Annotator,FileStack,OpModule,FileAnnosIn,FileAnnosTmp),
    execute_annotators(Annotators,FileStack,OpModule,FileAnnosTmp,FileAnnosOut).

execute_annotator(Annotator,FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    annotator(Annotator,Hooks,_),
    execute_annotator_hooks(Annotator,Hooks,FileStack,OpModule,FileAnnosIn,FileAnnosOut).

execute_annotator_hooks(_Annotator,[],_FileStack,_OpModule,FileAnnos,FileAnnos).
execute_annotator_hooks(Annotator,[Hook|Hooks],FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
    execute_annotator_hook(Annotator,Hook,FileStack,OpModule,FileAnnosIn,FileAnnosTmp),
    execute_annotator_hooks(Annotator,Hooks,FileStack,OpModule,FileAnnosTmp,FileAnnosOut).


%interleaved hooks are handled separately, see do_process_term and friends
execute_annotator_hook(_,interleaved,_,_,FileAnnos,FileAnnos).
execute_annotator_hook(Annotator,file,FileStack,OpModule,FileAnnosIn,FileAnnosOut):-
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

forget_file_annotation(Spec):-
    pdt_file_spec(Spec,FileName),
%    call_cleanup_hook(FileName),
    call_cleanup_hook2(FileName),
    retractall(file_annotation(FileName,_)),
    retractall(file_error(FileName,_,_)),
    retractall(file_comments(FileName,_,_)),
    clear_timestamp(FileName),
    clear_file_records(FileName),
    pif_notify(file_annotation(FileName),forget).



call_cleanup_hook2(FileName):-
    findall(Anotator,annotator(Anotator,_,_),Anotators),
    get_annos_for_cleanup(FileName,Annos),
    call_cleanup_hook2(Anotators,FileName,Annos).

get_annos_for_cleanup(FileName,Annos):-
    current_file_annotation(FileName,Annos),
    !.
get_annos_for_cleanup(_FileName,[]).


call_cleanup_hook2([],_,_).
call_cleanup_hook2([Annotator|Annotators],File,Annos):-
    pdt_maybe(
    	Annotator:cleanup_hook(File,Annos)
    ),
    call_cleanup_hook2(Annotators,File,Annos).     


/**
current_file_annotation(-Filename,-FileAnotations,-Terms)

 - Filename will be unified with an absolute file name
 - FileAnotations will be unified with a list of arbitrary annotations
   for that file.
 - Terms will be unified with a list of annotated terms, each one 
   annotated with arbitrary terms.
*/
current_file_annotation(FileSpec,FileAnotations):-
    nonvar(FileSpec),
    pdt_file_spec(FileSpec,Abs),    
    file_annotation(Abs,FileAnotations).
current_file_annotation(File,FileAnotations):-
    var(File),
    file_annotation(File,FileAnotations).    

current_file_error(FileSpec,Error):-
    pdt_file_spec(FileSpec,Abs),
    file_error(Abs,Error).

current_file_comments(FileSpec,Comments):-
    nonvar(FileSpec),
    pdt_file_spec(FileSpec,Abs),    
    file_comments(Abs,Comments).
current_file_comments(File,Comments):-
    var(File),
    file_comments(File,Comments).    

current_file_term(FileSpec,Term):-
    pdt_file_record_key(term,FileSpec,Key),
	pdt_file_record(Key,Term).

pdt_file_record(Key,Term):-
    recorded(Key,Term).

/*
pdt_annotator_context(+In,+Scope,-InMap,+OutMap,-Out).


all hook predicates use a set of accumulators which can be  
accessed and updated through this predicate.

In should be the input context as passed to the hook
Scope should be local,term,file, or global.
InMap will be unified with the current multimap for that scope
Out will be unified with a copy of in where the scope map is replaced by OutMap.

scope maps:
- global the 

*/

pdt_annotator_context(context(G,F,T,L0),local,L0,L1,context(G,F,T,L1)).
pdt_annotator_context(context(G,F,T0,L),term,T0,T1,context(G,F,T1,L)).
pdt_annotator_context(context(G,F0,T,L),file,F0,F1,context(G,F1,T,L)).
pdt_annotator_context(context(G0,F,T,L),global,G0,G1,context(G1,F,T,L)).

/*
convenience predicates: these are just shortcuts using the above ones
pdt_annotator_context_get(+In,+Scope,+Key,-val).
pdt_annotator_context_add(+In,+Scope,+Key,+val,-Out).

*/

pdt_annotator_context_get(In,Scope,Key,Val):-
    pdt_annotator_context(In,Scope,Map,_,_),
    pdt_multimap_get(Map,Key,Val).
    
/**
register_annotator(+FileSpec)

Filespec should be a file defining a module that defines the following 
predicates:

 - term_pre_annotation_hook(+FileStack,+OpModule,+InTerm,-OutTerm)
   	- FileStack is a stack containing the file names of 
   	  the files that are currently annotated. E.g. if file a loads
   	  file b and file b is the file currently to which the currently 
   	  annotated term belongs, the stack will be [b,a]. If the hook 
   	  implementation requires the annotation of another file c, 
   	  it should call ensure_annotated([c,b,a]). It is within the 
   	  responsibility of the hook implementation to avoid recursion by
   	  checking that the file c is not already on stack.
   	- OpModule is the module that is used by read_term for determining
   	  currently defined operators.
   	- InTerm is an annotated term that may already include an
   	  arbitrary number of annotations
   	- OutTerm should be unified with the same term including 
   	  the additional annotations the hook wishes to make.
   	
 - file_pre_annotation_hook(+FileStack,+OpModule,+Terms,+InAnos,-OutAnos)
    - FileStack is defined as above.
    - OpModule is defined as above.
    - Terms is a list of all (annotated) terms contained in the file
    - InAnos is a list of terms that where already attached by 
      other hooks.
    - OutAnos should be InAnos + the additions this hook whishes to make.
 - term_post_annotation_hook(+FileStack,+OpModule,+FileAnos,+InTerm,-OutTerm)
 	like term_pre_annotation/4, but is called after all file annotation hooks
 	have been processed.
  - file_post_annotation_hook(+FileStack,+OpModule,+Terms,+InAnos,-OutAnos) 	
  	like file_pre_annotation/5, but is called after term post processing.
*/

add_missing_hook(Module:Name/Arity):-
    (	Module:current_predicate(Name/Arity)
    ;	functor(Head,Name,Arity),
	    Module:assert(Head)
    ),
    !.

up_to_date(File):-
    time_file(File,Time),
    pdt_ht_get(pdt_annotation_time,File,Time).
update_timestamp(File):-
    time_file(File,Time),
    pdt_ht_put(pdt_annotation_time,File,Time).
clear_timestamp(File):-
    pdt_ht_remove_all(pdt_annotation_time,File).
/**
ensure_annotated(+FileSpec)

FileSpec should either be a single file specification or
a list of file specs.
Note: ensure_annotated/1 will only annotate the head entry of the list.
The tail is only used to pass along information on what files are
currently on stack, so that files that include each other do not lead
to infinite recursion.
*/
ensure_annotated(FileSpec):-
    \+ is_list(FileSpec),
	ensure_annotated([FileSpec]).
ensure_annotated([FileSpec|_]):-
    pdt_file_spec(FileSpec,Abs),
    up_to_date(Abs),
    !.
ensure_annotated([FileSpec|Stack]):- 
    new_memory_file(MemFile),
    pdt_file_spec(FileSpec,Abs),
    gen_op_module(Abs,OpModule),
    clear_ops(OpModule),
    retractall(file_annotation(Abs,_,_,_)),
    retractall(file_error(Abs,_,_)),
	update_timestamp(Abs),
    copy_file_to_memfile(FileSpec,MemFile),
    memory_file_to_atom(MemFile,MemFileAtom),
    open_memory_file(MemFile,read,Input),
    read_terms(MemFileAtom,[Abs|Stack],OpModule,Input),
    execute_annotators([Abs|Stack],OpModule,[],FileAnnos1),
	
%    collect_terms(FileStack,Terms),
    assert(file_annotation(Abs,FileAnnos1)),

	collect_comments([Abs|Stack],CommentsMap),
    assert(file_comments(Abs,CommentsMap)),

	collect_errors([Abs|Stack],Errors),
    (	nonvar(Errors),Errors\==[]
    ->	forall(member(Error,Errors),assert(file_error(Abs,Error)))
    ;	true
    ),
	close(Input),
	free_memory_file(MemFile),
	pif_notify(file_annotation(Abs),update),!.
ensure_annotated([FileSpec|_]):-    
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


do_process_term_rf(_Options,_MemFileAtom,_IAs,FileStack,N,end_of_file,_Error,N):-
	record_term(FileStack,end_of_file),
	!.	
do_process_term_rf(_Options,_MemFileAtom,_IAs,FileStack,N,_Term,Error,N):-
    nonvar(Error),!,
	record_error(FileStack,Error).
do_process_term_rf(Options,MemFileAtom,IAs,FileStack,N,Term0,_Error,M):-    
    member(subterm_positions(Positions),Options),	
	FileStack=[File|_],
	pdt_file_ref(File,FileRef),
	wrap_term(Term0,Positions,FileRef,N,Term1,M),   
	pdt_term_annotation(Term1,T,A),
	memberchk(variable_names(Names),Options),
	memberchk(singletons(Singletons),Options),	
	pdt_term_annotation(ProcessedTerm0,T,[variable_names(Names),singletons(Singletons)|A]),
	(	memberchk(comments(TermComments),Options)
    ->	comments_map(TermComments,CommentsMap),
    	process_comments(MemFileAtom,CommentsMap,ProcessedTerm0,Options,ProcessedTerm1),
    	record_comments(FileStack,TermComments)
    ;	ProcessedTerm0=ProcessedTerm1
    ),
	memberchk(module(OpModule),Options),
	execute_interleaved_hooks(IAs,FileStack,OpModule,ProcessedTerm1,ProcessedTerm),
    record_term(FileStack,ProcessedTerm).
		
record_error([File|_],Error):-
    file_key(error,File,Key),
    recordz(Key,Error).

record_comments([File|_],Comments):-
    file_key(comments,File,Key),
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

collect_terms([File|_],Terms):-
	file_key(term,File,Key),
    findall(Term,
    	(	recorded(Key,Term,Ref),
    		erase(Ref)
    	), Terms
    ).
    
collect_comments([File|_],CommentsMap):-
	file_key(comments,File,Key),
    findall(TermComments,
    	(	recorded(Key,TermComments,Ref),
    		erase(Ref)
    	), CommentsLists
    ),
    flatten(CommentsLists,Comments),
    comments_map(Comments,CommentsMap).

collect_errors([File|_],Errors):-
	file_key(error,File,Key),
    findall(Error,
    	(	recorded(Key,Error,Ref),
    		erase(Ref)
    	), Errors
    ).


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
do_file_key(comments,Ref,Key):-
    concat_atom([pdt_annotator,comments,Ref],'$',Key).



setup_options(Options):-
    pdt_preference_value(parse_comments,true),
    !,
    Options=[
		subterm_positions(_),
		variable_names(_),
		singletons(_),
    		module(_Module),
    		double_quotes(string),
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
	pdt_attach_comments(ATermIn,CommentsMap,Stream,CommentPositions,_,ATermOut),
	close(Stream),
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


		
get_op_module(FileSpec,OpModule):-
	pdt_file_spec(FileSpec,Abs),
	gen_op_module(Abs,OpModule).
    
gen_op_module(Abs,OpModule):-
    concat_atom([Abs,'$',op_module],OpModule).