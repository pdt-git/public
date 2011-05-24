:- module(metafile_referencer, [file_references_for_metacall/3,	%Arg1=ContextModule %Arg2=MetaCall %Arg3=References (see description)
								file_references_for_call/3,		%Arg1=ContextModule %Arg2=Term %Arg3=FileSet
								is_metaterm/3					%Arg1=ContextModule %Arg2=Literal %Arg3=MetaArgument
								]).
								
:- use_module(org_cs3_lp_utils(utils4modules)).

% FÜR EVA: Dies Prädikat müssen wir noch mal besprechen. -- G.

file_references_for_metacall(Module,MetaTerm,References):-
    is_metaterm(Module,MetaTerm,MetaArgs),			
    length(MetaArgs,Length),
    length(References,Length),
    nth1(N,MetaArgs,MArg),
    MArg=(ArgNr,Term),
	file_references_for_term(Module, Term, FileSet),
    nth1(N,References,(ArgNr,Term,FileSet)).
    
    
/* *
 * is_metaterm(-Literal, ?MetaArguments ) is non_det
 * is_metaterm(+Literal, ?MetaArguments ) is det
 *  Arg1 is a literal representing a metacall and 
 *  Arg2 is the list of its meta-arguments each of the form:
 *      (Original_argument_position, Argument).
 */
is_metaterm(Module, Literal, MetaArguments) :-
   visible_in_module(Module,Functor,Arity),
   functor(Literal,Functor,Arity),		
   predicate_property(Module:Literal,meta_predicate(MetaTerm)),
   Literal =.. [Functor|Args],
   MetaTerm =.. [Functor|MetaArgs],
   collect_meta_args(Args,MetaArgs, MetaArguments ).
    
    
/* *
* collect_meta_args(+Args,+MetaArgs,?MetaArguments) is det
* 
* MetaArguments is unified to a list of all elements of Args that are defined
* as meta-arguments via the corresponding elements of MetaArgs.
* (extract_meta_args/3 is used to select the corresponding elements and to 
* build the entries of MetaArguments.)
* Fails if no MetaArguments can be found.
*/
collect_meta_args(Args,MetaArgs, MetaArguments ) :- 
	bagof( 
        Meta,
        extract_meta_argument(Args,MetaArgs, Meta),
        MetaArguments
    ).
    
extract_meta_argument(Args,MetaArgs, (N,NewArg) ) :- 
    nth1(N,MetaArgs,MArg),
    nth1(N,Args,Arg),
    additonal_parameters(MArg,Arg,NewArg).

% If the meta-argument is not a variable,
% add as many parameters to it as indicated
% by the meta-argument specifier (0-9).
% Fail for (skip) parameters marked as ':'
% (= module-aware) but not as meta:
%additonal_parameters(':',Arg,Arg):- !.
additonal_parameters(0,Arg,Arg):- !.
additonal_parameters(N,Arg,Arg):-
    integer(N),
    var(Arg),!.
additonal_parameters(N,Arg,NewArg) :-
    integer(N),
	Arg =.. [Functor | Params],				
   	length(N_Elems,N),
   	append(Params,N_Elems,NewParams),
   	NewArg =.. [Functor | NewParams].
    
    
file_references_for_call(Module, Term, [(Module, 'any')]):-
    var(Term), !.
file_references_for_call(Module, Term, FileSet):-
    functor(Term,Name,Arity),
    findall( ContextFile,							
    		 (	defined_in_module(Module,Name,Arity),
    		    defined_in_file(Module,Name,Arity,_Nth,File,_Line),
    		 	ContextFile = (Module,File)
    		 ),
    		 Files
    ),
    not(Files = []), !,
    list_to_set(Files,FileSet).
file_references_for_call(Module, Term, FileSet):-
    functor(Term,Name,Arity),
    findall(	ContextFile,
    			(	(Module,Name,Arity,DeclModule),
    				module_property(DeclModule,file(File)),
    				predicate_property(DeclModule:Term,dynamic),
    				ContextFile = (Module,File)
    			),
    			Files
    ), 
    not(Files = []),  !,
    list_to_set(Files,FileSet).
file_references_for_call(Module, Term, [(Module, 'undefined')]):-
    functor(Term,Name,Arity),
	visible_in_module(Module,Name,Arity).   
