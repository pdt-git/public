/* ALLES HIER NOCH NICHT MIT DEM REST KONSISTENT. */

/* ***********************************************************************
   Wodurch ist eine Sprache beschrieben?
   ********************************************************************** */
 /* Unfertiges zeug:
   
   % Check completeness / consistency of language definition for language arg1.
language_definition(Language) :-
    ast_node_def(Language,Functor,ArgumentList),
    proper_Arg_List(Functor, ArgumentList),
    
proper_Arg_List(Functor, ArgumentList):-
    not( ArgumentList = [_,_,_|_] ),
    errorMsg( 'Argument list of ' Functor ' has less than three parameters.' ),
    !.
proper_Arg_List(Functor, [Arg|Rest]):-
    (Arg = ast(Functor,TypeList),
     ...


check_ast_sub_tree(Language, Name) :- 
	is_ast_argument(Language, Name).

is_ast_argument(Language, Name) :-
    ast_node_def(Language, Functor, Args),
    
    
generate_ast_attributes :-
    ast_node_def(Language,Label,Args),
    record_ast_attributes_(Args).
    
record_ast_attributes_([Arg|Rest]):-
    (Arg = ast_arg(Name,Cardinality,Types),
     !,
     record_ast_attribute_(Language,Name).
     
record_ast_attribute_([Arg|Rest])

    --------------------
*/
