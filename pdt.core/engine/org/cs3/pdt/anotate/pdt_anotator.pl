:- module(pdt_anotator,[
	ensure_anotated/1,
	register_anotator/1,
	current_file_anotation/3,
	forget_file_anotation/1
]).



:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).

:-dynamic file_anotation/4.
:-dynamic anotator/2.


forget_file_anotation(Spec):-
    pdt_file_spec(Spec,FileName),
    retractall(file_anotation(FileName,_,_,_)).

/**
current_file_anotation(-Filename,-FileAnotations,-Terms)

 - Filename will be unified with an absolute file name
 - FileAnotations will be unified with a list of arbitrary anotations
   for that file.
 - Terms will be unified with a list of anotated terms, each one 
   anotated with arbitrary terms.
*/
current_file_anotation(FileName,FileAnotations,Terms):-    
    file_anotation(FileName,_,FileAnotations,Terms).
    
/**
register_anotator(+FileSpec)

Filespec should be a file defining a module that defines the following 
predicates:

 - term_anotation_hook(+FileStack,+OpModule,+InTerm,-OutTerm)
   	- FileStack is a stack containing the file names of 
   	  the files that are currently anotated. E.g. if file a loads
   	  file b and file b is the file currently to which the currently 
   	  anotated term belongs, the stack will be [b,a]. If the hook 
   	  implementation requires the anotation of another file c, 
   	  it should call ensure_anotated([c,b,a]). It is within the 
   	  responsibility of the hook implementation to avoid recursion by
   	  checking that the file c is not already on stack.
   	- OpModule is the module that is used by read_term for determining
   	  currently defined operators.
   	- InTerm is an anotated term that may already include an
   	  arbitrary number of anotations
   	- OutTerm should be unified with the same term including 
   	  the additional anotations the hook wishes to make.
   	
 - file_anotation_hook(+FileStack,+OpModule,+Terms,+InAnos,-OutAnos)
    - FileStack is defined as above.
    - OpModule is defined as above.
    - Terms is a list of all (anotated) terms contained in the file
    - InAnos is a list of terms that where already attached by 
      other hooks.
    - OutAnos should be InAnos + the additions this hook whishes to make.

*/
register_anotator(File):-
    anotator(File,_),!.
register_anotator(File):-
    use_module(File),
    pdt_file_spec(File,Abs),
    current_module(Anotator,Abs),
    assert(anotator(File,Anotator)).

/**
ensure_anotated(+FileSpec)

FileSpec should either be a single file specification or
a list of file specs.
Note: ensure_anotated/1 will only anotate the head entry of the list.
The tail is only used to pass along information on what files are
currently on stack, so that files that include each other do not lead
to infinite recursion.
*/
ensure_anotated(FileSpec):-
    \+ is_list(FileSpec),
    ensure_anotated([FileSpec]).
ensure_anotated([FileSpec|_]):-
    pdt_file_spec(FileSpec,Abs),
    time_file(Abs,Time),
    file_anotation(Abs,Time,_,_),    
    !.
ensure_anotated([FileSpec|Stack]):-    
    new_memory_file(MemFile),
    pdt_file_spec(FileSpec,Abs),
    retractall(file_anotation(Abs,_,_,_)),
    time_file(Abs,Time),
    copy_file_to_memfile(FileSpec,MemFile),
    open_memory_file(MemFile,read,Input),
    gen_op_module(Abs,OpModule),
    read_stream_to_wraped_terms([Abs|Stack],OpModule,Input,Terms),
    process_file([Abs|Stack],OpModule,Terms,FileAnos),
    assert(file_anotation(Abs,Time,FileAnos,Terms)),
	close(Input),
	free_memory_file(MemFile).
	
read_stream_to_wraped_terms(FileStack,OpModule,In,Terms):-
    	read_term(In,Term,[
    		subterm_positions(Positions),
    		module(OpModule),
    		double_quotes(string)]),
    	(	Term==end_of_file
    	->	Terms=[]
    	;	Terms=[H|T],    		
    		wrap_term(Term,Positions,WrapedTerm),   
    		process_term(FileStack,OpModule,WrapedTerm,H),
			read_stream_to_wraped_terms(FileStack,OpModule,In,T)
		).

process_term(FileStack,OpModule,InTerm,OutTerm):-
    findall(Anotator,anotator(_,Anotator),Anotators),
    process_term(Anotators,FileStack,OpModule,InTerm,OutTerm).
    
process_term([],_,_,Term,Term). 
process_term([Anotator|T],FileStack,OpModule,InTerm,OutTerm):-
    pdt_maybe(
    	Anotator:term_anotation_hook(FileStack,OpModule,InTerm,TmpTerm)
    ),
    (	var(TmpTerm)
    ->	process_term(T,FileStack,OpModule,InTerm,OutTerm)
    ;	process_term(T,FileStack,OpModule,TmpTerm,OutTerm)
    ).
	

process_file(FileStack,OpModule,Terms,FileAnos):-
	findall(Anotator,anotator(_,Anotator),Anotators),
    process_file(Anotators,FileStack,OpModule,Terms,[],FileAnos).
process_file([],_,_,_,Terms,Terms). 
process_file([Anotator|T],FileStack,OpModule,Terms,InAnos,OutAnos):-
    pdt_maybe(
	    Anotator:file_anotation_hook(FileStack,OpModule,Terms,InAnos,TmpAnos)
	),
	process_file(T,FileStack,OpModule,Terms,TmpAnos,OutAnos).

		
wrap_term(Term,Positions,aterm([position(From-To)|_],SubTerm)):-
    compound(Term),
    !,
    Term=..[Functor|Args],
    top_position(Positions, From,To),
    sub_positions(Positions,SubPositions),
    wrap_args(Args,SubPositions,WrapedArgs),
    SubTerm=..[Functor|WrapedArgs].
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