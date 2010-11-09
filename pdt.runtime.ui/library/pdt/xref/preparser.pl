:- module(preparser, [parse/1, parse/2]).

:- ensure_loaded(parse_util).
:- use_module(directive_handler).
 
/**
 * parse(?File)
 * 	opens a stream to the file Arg1, tries to parse it as a prolog program and
 * 	closes the stream afterwards. 
 */
parse(File):-
    open(File,read,InStream),
    parse(File,InStream),
    close(InStream).
      
/**
 * parse(+File, +InStream)
 * 	creates some unic facts for the file Arg1 and starts the parsing of the
 * 	clauses contained in the stream Arg2 (which should be one to the file Arg1).
 */          
parse(File,InStream):-
    new_node_id(Id),	
    nb_setval(module_to_parse, user),
    parse_clauses(InStream,Id),
    nb_getval(module_to_parse,ActualModule),
    assert(fileT(Id,File,ActualModule)),    
    assert(fileT_ri(File,Id)).

/**
 * parse_clause(+InStream,+FileId)
 *	  reads the first term from Arg1 if it is not EoF,
 *    parses possible subterms recursively and asserts some unic facts about the term.
 */
parse_clauses(InStream,FileId):-
    repeat,
    	catch(
        	read_term(InStream,Clause,
            	[   %term_position(Pos),        % output
              		subterm_positions(SubPos), % output
%                	module(CurrentModule),     % INput
%                	singletons(Singletons),     % output
                	variable_names(VarNames)  % output
            	]),
        	error(Error,Context),
        	( assert(error(Error,Context,FileId)),  % <<<<
          	fail											%ToDo: eventually change this
        	)  
    	),  
     	(   Clause==end_of_file
    	->  true
    	;   nb_getval(module_to_parse,Module),
        	numbervars(VarNames,0,_),
        	parse_clause_elements(Clause,SubPos,FileId,VarNames,Module),
 %       	assert(node_attr(ClauseId,singletons,Singletons))   
 			fail
    	),
    !.
/**
 * ToDo: safe singeltons if needed,
 *       rewrite discription
 */
    
/**
 * parse_clause_elements(+Term,+Pos,+Parent,+VarNames,+Module)
 *    parses the term given in Arg1 to PEFs. The term has to be a clause.
 *    If it is a
 *    -  directive, a directiveT is defined and its body is parsed.
 *    -  fact, a factT and a clauseT_ri (for reverse indexing) are defined
 *    -  rule, a clauseT and a clauseT_ri (for reverse indexing) are defined, 
 			its body is parsed.
 *    Body elements are parsed with parse_body_literals/6, head elements with
 *    parse_head_literals/5.
 *    Additionally the remaining arguments are used to define slTs for the
 *    corresponding position information.
 **/
parse_clause_elements(:- Body, Pos, FileId, VarNames, UnchangedModule) :- % Directive
   	!, 
   	Pos = term_position(From, To, _FFrom, _FTo, [SubPos]),
   	assert_new_node( :- Body,From,To,ClauseId), 
   	Body =..[Functor|Args],
   	handle_directive(Functor,Args,SubPos,ClauseId,FileId,UnchangedModule),
   	nb_getval(module_to_parse,Module),
   	assert(directiveT(ClauseId,FileId,Module)),
   	Body =.. [Functor|Args],
 	assert(pos_and_vars(ClauseId,SubPos,VarNames)).
   
parse_clause_elements(Head :- Body, Pos, FileId, VarNames, Module) :-  % Rule
	!, 
	Pos = term_position(From, To, _FFrom, _FTo, SubPos),
 	SubPos = [HeadPos, BodyPos], 
	assert_new_node(Head :- Body,From,To,ClauseId),  
	functor(Head,Functor,Arity),
    assert(clauseT(ClauseId,FileId,Module,Functor,Arity)),
    assert(clauseT_ri(Functor,Arity,ClauseId)),	
	assert(pos_and_vars(ClauseId,BodyPos,VarNames)),
    parse_head_literal(Head, HeadPos, ClauseId, Module, VarNames).

parse_clause_elements(Head, Pos, FileId, VarNames, Module) :-   % Fact
   	!, 					
   	(	Pos = term_position(From, To, _FFrom, _FTo, _SubPos)
   	;	Pos = From - To
   	),
   	assert_new_node(Head,From,To,ClauseId), 
	functor(Head,Functor,Arity),
    assert(clauseT(ClauseId,FileId,Module,Functor,Arity)),
    assert(clauseT_ri(Functor,Arity,ClauseId)),
    parse_head_literal(Head, Pos, ClauseId, Module, VarNames).    
 
 
 
 
parse_head_literal(Module:Head, Pos, ClauseId, _OrigModule, _VarNames) :-
    !,
    Pos = term_position(From, To, _FFrom, _FTo, _SubPos),
 %   SubPos = [ModuleFrom-ModuleTo, HeadPos],
   	assert_new_node(Module:Head,From,To,Id),   %<===   
   	functor(Head,Functor,Arity),
   	assert(headT(Id,ClauseId,ClauseId,Module,Functor,Arity)).    

/******** 
 * Eva: Was genau soll in dem Fall da oben gespeichert werden?
 ********/
   	
parse_head_literal(Head, Pos, ClauseId, Module, _VarNames) :-
   	(	Pos = term_position(From, To, _FFrom, _FTo, _SubPos)
   	;	Pos = From - To
   	),
   	assert_new_node(Head,From,To,Id),   %<===   
   	functor(Head,Functor,Arity),
   	assert(headT(Id,ClauseId,ClauseId,Module,Functor,Arity)). 
 
