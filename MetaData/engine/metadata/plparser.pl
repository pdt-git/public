:- module(plparser,[parse/1,parse/2,node_attr/2]). 
:- dynamic node_attr/2.
:- dynamic node_id/1.

node_attr(source_folder_node(P),child(C)):-
    node_attr(source_folder_node(P),compilation_unit(C)).
node_attr(compilation_unit_node(P),child(C)):-
    node_attr(compilation_unit_node(P),member(C)).
node_attr(brace_node(P),child(C)):-
    node_attr(brace_node(P),argument(C)).    
node_attr(list_node(P),child(C)):-
    node_attr(list_node(P),elements(L)),
    member(C,L).            
node_attr(compound_node(P),child(C)):-
    node_attr(compound_node(P),arguments(L)),
    member(C,L).            
parse(File,InStream):-
    writeln(ta),
    unused_id(compilation_unit,Id),
    my_assert(node_attr(Id,type(compilation_unit))),
    my_assert(node_attr(Id,file(File))),
   	parse_clauses(InStream,Id,user).


parse(File):-
    writeln(tu),
    open(File,read,InStream),
    parse(File,InStream),
    close(InStream).


parse_clauses(InStream,FileId,CurrentModule):-    
	parse_clause(InStream,FileId,CurrentModule,NextModule,ClauseId),
	(	ClauseId==end_of_file
	->	true
	;	my_assert(node_attr(FileId,member(ClauseId))),
		parse_clauses(InStream,FileId,NextModule)
	).

    

parse_clause(InStream,ParentId,CurrentModule,NextModule,ClauseId):-
	catch(
		read_term(InStream,Term,
			[	term_position(Pos),
				subterm_positions(Sub),
				module(CurrentModule),
				variable_names(VarNames),
				singletons(Singletons)
			]),
		error(Error,Context),
		(handle_error(ClauseId,Error,Context),fail)
	),	
    format("processing ~w~n",[Term]),
	arg(2,Pos,Line)	,
	(	Term==end_of_file
	->	ClauseId=end_of_file
	;   next_module(Term,NextModule),	    
		b_setval(bindings,VarNames),
		parse_subterm_positions(ParentId,Term,Sub,ClauseId),
	   	my_assert(node_attr(ClauseId,line(Line))),      
		my_assert(node_attr(ClauseId,toplevel_term)),
		my_assert(node_attr(ClauseId,singletons(Singletons))),
		my_assert(node_attr(ClauseId,module(CurrentModule)))
	).



next_module(Term,NextModule):-
    Term=..[:-,Directive],
    Directive=..[module,NextModule,_],
    !.
next_module(_,user).
    
handle_error(Id,Error,Context,File):-
    unused_id(error,Id),
	my_assert(node_attr(Id,error(Error))),    
	my_assert(node_attr(Id,context(Context))),
	my_assert(node_attr(Id,file(File))).
	
assert_common_attrs(Id,Type,Parent,From-To,Term):-
    my_assert(node_attr(Id,type(Type))),
    my_assert(node_attr(Id,parent(Parent))),
    my_assert(node_attr(Id,From-To)),
    my_assert(node_attr(Id,term(Term))).
var_name(Var,Name):-
	b_getval(bindings,B),
	(	member(Name=C,B),C==Var   
	->	true
	;	C='_'
	).
	
parse_subterm_positions(ParentId,Term, From-To,Id):-	
	( ground(Term)
	->unused_id(atom,Id),
	  assert_common_attrs(Id,atom,ParentId,From-To,Term)
	; unused_id(variable,Id),
	  assert_common_attrs(Id,variable,ParentId,From-To,Term),
	  var_name(Term,Name),
	  my_assert(node_attr(Id,name(Name)))
	).
	
parse_subterm_positions(ParentId,Term, string_position(From,To),Id):-
	unused_id(string,Id),
	assert_common_attrs(Id,string,ParentId,From-To,Term).

parse_subterm_positions(ParentId,Term,  brace_term_position(From, To, Arg),Id):-
    unused_id(brace,Id),
	assert_common_attrs(Id,brace,ParentId,From-To,Term),
	arg(1,Term,ArgTerm),
	parse_subterm_positions(Id,ArgTerm,Arg,ArgId),
	my_assert(node_attr(Id,argument(ArgId))).

parse_subterm_positions(ParentId,Term,  list_position(From, To, Elms, Tail),Id):-
   	unused_id(list,Id),
	assert_common_attrs(Id,list,ParentId,From-To,Term),
	parse_elm_positions(Id,Term,Elms,ElmIDs,TailTerm),
	my_assert(node_attr(Id,elements(ElmIDs))),
	(	Tail==none
	->	true
	;	parse_subterm_positions(Id,TailTerm,Tail,TailId),
		my_assert(node_attr(Id,tail(TailId)))
	).
    
parse_subterm_positions(ParentId,Term,  term_position(From, To, FFrom, FTo, SubPos),Id):-
    unused_id(compound,Id),
    assert_common_attrs(Id,compound,ParentId,From-To,Term),
    my_assert(node_attr(Id,functor_position(FFrom-FTo))),
    functor(Term,Name,Arity),
    my_assert(node_attr(Id,functor(Name/Arity))),
    Term=..[_|Args],
   	parse_arg_positions(Id,Args,SubPos,ArgIDs),
   	my_assert(node_attr(Id,arguments(ArgIDs))).

parse_arg_positions(_,[],[],[]).   	

parse_arg_positions(ParentId,[Arg|Args],[Position|Positions],[Id|Ids]):-
   	parse_subterm_positions(ParentId,Arg,Position,Id),
   	parse_arg_positions(ParentId,Args,Positions,Ids).

%parse_elm_positions(+Parent,+Term,+ElmPositions,-ElmIds,-TailTerm)
parse_elm_positions(_,TailTerm,[],[],TailTerm).   	

parse_elm_positions(ParentId,[HeadTerm|RestTerm],[HeadPos|RestPos],[HeadId|RestIds],TailTerm):-
   	parse_subterm_positions(ParentId,HeadTerm,HeadPos,HeadId),
   	parse_elm_positions(ParentId,RestTerm,RestPos,RestIds,TailTerm).
   	

my_assert(node_id(Id)):-
    ( 	node_id(Id)
	;   assert(node_id(Id))
	).

my_assert(node_attr(Id,Attr)):-
    my_assert(node_id(Id)),
    (	node_attr(Id,Attr)
    ;	assert(node_attr(Id,Attr))
    ).

unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	
