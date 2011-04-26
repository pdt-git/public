/**
 * just for testing something with directives
 **/

:- consult(walking_prolog_files).

collect_directives(Dir):-
    prolog_files(Dir, Files),
    collect_for_files(Files,_,Directives),
    print_directives(Directives).
    
collect_for_files([File|Files],FormerDirectives,AllDirectives):-
    collect_for_files(Files,FormerDirectives,SomeDirectives),
    collect_for_a_file(File,SomeDirectives,AllDirectives).
collect_for_files([],_,[]).
    
collect_for_a_file(File,Former,All):-
    open(File,read,InStream),
    collect_from_term(InStream,[],Found),
    append(Found,Former,AllFound),
    list_to_set(AllFound,All),		
    close(InStream).
   
/*collect_from_term(Stream,Former,All):-
    read_term(Stream,Clause,[syntax_errors(dec10)]),
    (	Clause == end_of_file
    ->	All=Former
    ;	(	(	Clause =.. [(:-),Directive|_Rest],
    			Directive =.. [Functor|_]
   			;	Functor=''
   			),
   			collect_from_term(Stream,Former,Found),
   			All = [Functor|Found]
   		)
   	).*/
collect_from_term(Stream,Former,All):-
    read_term(Stream,Clause,[syntax_errors(dec10)]),
    (	Clause == end_of_file
    ->	All=Former
    ;	(	(	Clause =.. [(:-),Directive|_Rest],
    			Directive =.. [reexport|Args]
   			;	Args=''
   			),
   			collect_from_term(Stream,Former,Found),
   			All = [Args|Found]
   		)
   	).

    
print_directives([Directive|Directies]):-
    writeln(Directive),
    print_directives(Directies).
