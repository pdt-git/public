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

:-dynamic file_annotation/4.
:-dynamic file_error/3.
:-dynamic annotator/2.


forget_file_annotation(Spec):-
    pdt_file_spec(Spec,FileName),
    retractall(file_annotation(FileName,_,_,_)),
    retractall(file_error(FileName,_,_)).

/**
current_file_annotation(-Filename,-FileAnotations,-Terms)

 - Filename will be unified with an absolute file name
 - FileAnotations will be unified with a list of arbitrary annotations
   for that file.
 - Terms will be unified with a list of annotated terms, each one 
   annotated with arbitrary terms.
*/
current_file_annotation(FileSpec,FileAnotations,Terms):-
    pdt_file_spec(FileSpec,Abs),    
    file_annotation(Abs,_,FileAnotations,Terms).

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
    assert(annotator(File,Anotator)).

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
    time_file(Abs,Time),
    file_annotation(Abs,Time,_,_),    
    !.
ensure_annotated([FileSpec|Stack]):-    
    new_memory_file(MemFile),
    pdt_file_spec(FileSpec,Abs),
    gen_op_module(Abs,OpModule),
    clear_ops(OpModule),
    retractall(file_annotation(Abs,_,_,_)),
    retractall(file_error(Abs,_,_)),
    time_file(Abs,Time),
    copy_file_to_memfile(FileSpec,MemFile),
    open_memory_file(MemFile,read,Input),
    read_terms([Abs|Stack],OpModule,Input,Terms,Errors),
    pre_process_file([Abs|Stack],OpModule,Terms,FileAnos),
    post_process_terms([Abs|Stack],OpModule,FileAnos,Terms,OutTerms),
    post_process_file([Abs|Stack],OpModule,OutTerms,FileAnos,OutAnos),
    assert(file_annotation(Abs,Time,OutAnos,OutTerms)),
    (	Errors\==[]
    ->	forall(member(Error,Errors),assert(file_error(Abs,Time,Error)))
    ;	true
    ),
	close(Input),
	free_memory_file(MemFile).

clear_ops(OpModule):-
    forall(current_op(P,T,OpModule:N),
    		(	current_op(P,T,user:N)
    		->	true
    		;	op(0,T,OpModule:N)
    		)
    	).
	
%read_stream_to_wraped_terms(FileStack,OpModule,In,Terms,Errors):-
%	catch(
%		( read_term(In,Term,[subterm_positions(Positions),module(OpModule),double_quotes(string)]),
%		  Errors=ET
%		),
%		E,
%		Errors=[E|ET]
%	),
%    	(	Term==end_of_file
%    	->	Terms=[],
%    		Errors=[]%Errors and ET should be sharing variables at this point
%    	;	(	nonvar(Term)
%    		->	Terms=[H|T],    		
%	    		wrap_term(Term,Positions,WrapedTerm),   
%    			pre_process_term(FileStack,OpModule,WrapedTerm,H),
%			read_stream_to_wraped_terms(FileStack,OpModule,In,T,ET)
%    		;	read_stream_to_wraped_terms(FileStack,OpModule,In,Terms,ET)
%    		)
%	).


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


do_process_term(_,_,_,end_of_file,_,_,[],[]):-!.
do_process_term(FileStack,Module,In,_,_,Error,Terms,[Error|Errors]):-
    nonvar(Error),!,
    read_terms_rec(FileStack,Module,In,Terms,Errors).
do_process_term(FileStack,OpModule,In,Term,Options,_,[ProcessedTerm|Terms],Errors):-    
	member(subterm_positions(Positions),Options),
	wrap_term(Term,Positions,WrapedTerm),   
    	pre_process_term(FileStack,OpModule,WrapedTerm,ProcessedTerm),
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
    	Anotator:current_predicate(term_pre_annotation_hook/4),
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
    	Anotator:current_predicate(term_post_annotation_hook/5),
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
	    Anotator:current_predicate(file_pre_annotation_hook/5),
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
	    Anotator:current_predicate(file_post_annotation_hook/5),    
	    Anotator:file_post_annotation_hook(FileStack,OpModule,Terms,InAnos,TmpAnos)
	)),
	(	var(TmpAnos)
	->	post_process_file(T,FileStack,OpModule,Terms,InAnos,OutAnos)
	;	post_process_file(T,FileStack,OpModule,Terms,TmpAnos,OutAnos)
	).



		
wrap_term(Term,Positions,aterm([position(From-To)|_],SubTerm)):-
    compound(Term),
    !,
    Term=..[Functor|Args],
    top_position(Positions, From,To),
    sub_positions(Positions,SubPositions),
    functor(Term,Functor,Arity),
    functor(SubTerm,Functor,Arity),
    SubTerm=..[Functor|WrapedArgs],
    wrap_args(Args,SubPositions,WrapedArgs).
wrap_term(Term,Positions,aterm([position(From-To)|_],Term)):-
    \+ compound(Term),
    top_position(Positions, From,To),!.
wrap_term(Term,_,aterm([implicit_nil|_],Term)):-
    \+ compound(Term).    

wrap_args([],[],[]).
wrap_args([H|T],[PH|PT],[WH|WT]):-
    wrap_term(H,PH,WH),
    wrap_args(T,PT,WT).
    
    
gen_op_module(Abs,OpModule):-
    concat_atom([Abs,'$',op_module],OpModule).