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
	register_annotator/1,
	current_file_annotation/3,
	current_file_error/2,
	forget_file_annotation/1
]).



:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('org/cs3/pdt/util/pdt_util_hashtable')).
:- use_module(library('pif_observe')).

:-dynamic file_annotation/4.
:-dynamic file_error/3.
:-dynamic annotator/2.


forget_file_annotation(Spec):-
    pdt_file_spec(Spec,FileName),
    retractall(file_annotation(FileName,_,_,_)),
    retractall(file_error(FileName,_,_)),
    clear_timestamp(FileName),
    pif_notify(file_annotation(FileName),forget).

/**
current_file_annotation(-Filename,-FileAnotations,-Terms)

 - Filename will be unified with an absolute file name
 - FileAnotations will be unified with a list of arbitrary annotations
   for that file.
 - Terms will be unified with a list of annotated terms, each one 
   annotated with arbitrary terms.
*/
current_file_annotation(FileSpec,FileAnotations,Terms):-
    nonvar(FileSpec),
    pdt_file_spec(FileSpec,Abs),    
    file_annotation(Abs,_,FileAnotations,Terms).
current_file_annotation(File,FileAnotations,Terms):-
    var(File),
    file_annotation(File,_,FileAnotations,Terms).    

current_file_error(FileSpec,Error):-
    pdt_file_spec(FileSpec,Abs),
    file_error(Abs,_,Error).
    
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
register_annotator(File):-
    annotator(File,_),!.
register_annotator(File):-
    use_module(File),
    pdt_file_spec(File,Abs),
    current_module(Anotator,Abs),
    add_missing_hooks(Anotator),
    assert(annotator(File,Anotator)).


add_missing_hooks(Anotator):-
    add_missing_hook(Anotator:term_pre_annotation_hook/4),
    add_missing_hook(Anotator:term_post_annotation_hook/5),
    add_missing_hook(Anotator:file_pre_annotation_hook/5),
    add_missing_hook(Anotator:file_post_annotation_hook/5).

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
    open_memory_file(MemFile,read,Input),
    read_terms([Abs|Stack],OpModule,Input,Terms,Errors),
    pre_process_file([Abs|Stack],OpModule,Terms,FileAnos),
    post_process_terms([Abs|Stack],OpModule,FileAnos,Terms,OutTerms),
    post_process_file([Abs|Stack],OpModule,OutTerms,FileAnos,OutAnos),
    assert(file_annotation(Abs,Time,OutAnos,OutTerms)),
    (	nonvar(Errors),Errors\==[]
    ->	forall(member(Error,Errors),assert(file_error(Abs,Time,Error)))
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
	


read_terms(FileStack,OpModule,In,Terms,Errors):-
    
    	read_terms_rec(FileStack,OpModule,In,Terms,Errors).
    	
read_terms_rec(FileStack,Module,In,Terms,Errors):-
    Options=[
		subterm_positions(_),
		variable_names(_),
		singletons(_),
    		module(Module),
    		double_quotes(string)
    	],
    do_read_term(In,Term,Options,Error),
    do_process_term(FileStack,Module,In,Term,Options,Error,Terms,Errors).


do_process_term(_,_,_,Term,_,_,[],[]):-
	Term==end_of_file,
	!.
do_process_term(FileStack,Module,In,_,_,Error,Terms,[Error|Errors]):-
    nonvar(Error),!,
    read_terms_rec(FileStack,Module,In,Terms,Errors).
do_process_term(FileStack,OpModule,In,Term0,Options,_,[ProcessedTerm|Terms],Errors):-    
	member(subterm_positions(Positions),Options),
	wrap_term(Term0,Positions,Term1),   
	%pdt_term_annotation(Term1,T,A),
	memberchk(variable_names(Names),Options),
	memberchk(singletons(Singletons),Options),	
	%pdt_term_annotation(Term2,T,[variable_names(Names),singletons(Singletons)|A]),
   	pdt_add_annotation(Term1,variable_names,Names,Term2),
   	pdt_add_annotation(Term2,singletons,Singletons,Term3),
   	pre_process_term(FileStack,OpModule,Term3,ProcessedTerm),
    read_terms_rec(FileStack,OpModule,In,Terms,Errors).

do_read_term(In,Term,Options,Error):-
    catch(
		read_term(In,Term,Options),
		Error,
		true
	).

pre_process_term(FileStack,OpModule,InTerm,OutTerm):-
    findall(Anotator,annotator(_,Anotator),Anotators),
    pre_process_term(Anotators,FileStack,OpModule,InTerm,OutTerm).
    
pre_process_term([],_,_,Term,Term). 
pre_process_term([Anotator|T],FileStack,OpModule,InTerm,OutTerm):-
    pdt_maybe((
    	Anotator:term_pre_annotation_hook(FileStack,OpModule,InTerm,TmpTerm)
    )),
    (	var(TmpTerm)
    ->	pre_process_term(T,FileStack,OpModule,InTerm,OutTerm)
    ;	pre_process_term(T,FileStack,OpModule,TmpTerm,OutTerm)
    ).

post_process_terms(_,_,_,[],[]).
post_process_terms(Stack,OpModule,FileAnos,[InHead|InTail],[OutHead|OutTail]):-
    post_process_term(Stack,OpModule,FileAnos,InHead,OutHead),
    post_process_terms(Stack,OpModule,FileAnos,InTail,OutTail).
	
post_process_term(FileStack,OpModule,FileAnos,InTerm,OutTerm):-
    findall(Anotator,annotator(_,Anotator),Anotators),
    post_process_term(Anotators,FileStack,OpModule,FileAnos,InTerm,OutTerm).

post_process_term([],_,_,_,Term,Term). 
post_process_term([Anotator|T],FileStack,OpModule,FileAnos,InTerm,OutTerm):-
    pdt_maybe((
    	Anotator:term_post_annotation_hook(FileStack,OpModule,FileAnos,InTerm,TmpTerm)
    )),
    (	var(TmpTerm)
    ->	post_process_term(T,FileStack,OpModule,FileAnos,InTerm,OutTerm)
    ;	post_process_term(T,FileStack,OpModule,FileAnos,TmpTerm,OutTerm)
    ).


pre_process_file(FileStack,OpModule,Terms,FileAnos):-
	findall(Anotator,annotator(_,Anotator),Anotators),
    pre_process_file(Anotators,FileStack,OpModule,Terms,[],FileAnos).
pre_process_file([],_,_,_,Terms,Terms). 
pre_process_file([Anotator|T],FileStack,OpModule,Terms,InAnos,OutAnos):-
    pdt_maybe((
	    Anotator:file_pre_annotation_hook(FileStack,OpModule,Terms,InAnos,TmpAnos)
	)),
	(	var(TmpAnos)
	->	pre_process_file(T,FileStack,OpModule,Terms,InAnos,OutAnos)
	;	pre_process_file(T,FileStack,OpModule,Terms,TmpAnos,OutAnos)
	).

post_process_file(FileStack,OpModule,Terms,InAnos,OutAnos):-
	findall(Anotator,annotator(_,Anotator),Anotators),
    post_process_file(Anotators,FileStack,OpModule,Terms,InAnos,OutAnos).
post_process_file([],_,_,_,Terms,Terms). 
post_process_file([Anotator|T],FileStack,OpModule,Terms,InAnos,OutAnos):-
    pdt_maybe((
	    Anotator:file_post_annotation_hook(FileStack,OpModule,Terms,InAnos,TmpAnos)
	)),
	(	var(TmpAnos)
	->	post_process_file(T,FileStack,OpModule,Terms,InAnos,OutAnos)
	;	post_process_file(T,FileStack,OpModule,Terms,TmpAnos,OutAnos)
	).



		

    
    
gen_op_module(Abs,OpModule):-
    concat_atom([Abs,'$',op_module],OpModule).