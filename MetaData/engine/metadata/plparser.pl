/**
the module plarse provides a means of parsing prolog source into an
abstract syntax tree. It makes use of the model's basic modification api
and stores the ast structures using source nodes
*/
:- module(plparser,[
	parse/1,
	parse/2
]). 

:- use_module(model).


parse(File,InStream):-
    create_node(compilation_unit,Id),
	add_node_property(Id,file(File)),
    source_folder(FolderId,File),
    connect_nodes(FolderId,compilation_unit,Id,parent),
   	parse_clauses(Id,File,InStream,user).


parse(File):-
    open(File,read,InStream),
    parse(File,InStream),
    close(InStream).



source_folder(FolderId,File):-
    file_directory_name(File,Dir),
    (	(	node_property(FolderId,file(Dir)),
			node_property(FolderId,type(source_folder))
		)
	->	true		
	;	create_node(source_folder,FolderId),
		add_node_property(FolderId,file(Dir))
	).

parse_clauses(FileId,File,InStream,Module):-
	parse_clause(Id,File,Module,InStream,NextModule),
	!,
	connect_nodes(FileId,clause,Id,parent),
    !,
	parse_clauses(FileId,File,InStream,NextModule).

    

parse_clause(Id,File,Module,InStream,NextModule):-
	catch(
		read_term(InStream,Term,[term_position(Pos),subterm_positions(Sub),module(Module)]),
		error(Error,Context),
		(handle_error(Id,Error,Context,File),fail)
	),	
	arg(2,Pos,Line)	,
	!,
	\+ Term==end_of_file,
	next_module(Term,NextModule),	    
	parse_subterm_positions(Id,Term,Sub),
   	add_node_property(Id,file(File)),
	add_node_property(Id,line(Line)),      
	add_node_property(Id,clause).



next_module(Term,NextModule):-
    Term=..[:-,Directive],
    Directive=..[module,NextModule,_],
    !.
next_module(_,user).
    
handle_error(Id,Error,Context,File):-
	add_node_property(Id,error(Error)),    
	add_node_property(Id,context(Context)),
	add_node_property(Id,file(File)).
	
parse_subterm_positions(Id,Term, From-To):-
	( term_variables(Term,[])
	->create_node(atom,Id)
	; create_node(variable,Id)
	),
	add_node_property(Id,position(From-To)),
	add_node_property(Id,term(Term)).
	
parse_subterm_positions(Id,Term, string_position(From,To)):-
	create_node(string,Id),
	add_node_property(Id,position(From-To)),
	add_node_property(Id,term(Term)).

parse_subterm_positions(Id,Term,  brace_term_position(From, To, Arg)):-
    create_node(brace,Id),
	add_node_property(Id,position(From-To)),
	add_node_property(Id,term(Term)),
	arg(1,Term,ArgTerm),
	parse_subterm_positions(ArgId,ArgTerm,Arg),
	add_node_property(Id,term(Term)),
	connect_nodes(Id,argument,ArgId,parent).
 
parse_subterm_positions(Id,Term,  list_position(From, To, Elms, Tail)):-
   	create_node(list,Id),
	add_node_property(Id,position(From-To)),
	add_node_property(Id,term(Term)),
	parse_elm_positions(Id,Term,Elms,Tail).
    
parse_subterm_positions(Id,Term,  term_position(From, To, FFrom, FTo, SubPos)):-
    create_node(compound,Id),
	add_node_property(Id,position(From-To)),
	add_node_property(Id,functor_position(FFrom-FTo)),
    functor(Term,Name,Arity),
	add_node_property(Id,functor(Name/Arity)),
	add_node_property(Id,term(Term)),
    Term=..[_|Args],
   	parse_arg_positions(Id,Args,SubPos).
%  	my_assert(parsed_node_attr(Id,arguments(ArgIDs))).

parse_arg_positions(_,[],[]).   	

parse_arg_positions(ParentId,[Arg|Args],[Position|Positions]):-
   	parse_subterm_positions(Id,Arg,Position),
   	connect_nodes(ParentId,arguments,Id,parent),
   	parse_arg_positions(ParentId,Args,Positions).

parse_elm_positions(_,[],[],_,[],_).   	

parse_elm_positions(ParentId,Elm,[],TailPosition):-
   	parse_subterm_positions(TailId,Elm,TailPosition),
   	connect_nodes(ParentId,tail,TailId,parent).

   	
parse_elm_positions(ParentId,[Elm|Elms],[Position|Positions],TailPosition):-
   	parse_subterm_positions(Id,Elm,Position),
	connect_nodes(ParentId,elements,Id,parent),
   	parse_elm_positions(ParentId,Elms,Positions,TailPosition).


