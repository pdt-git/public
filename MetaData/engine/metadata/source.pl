:- module(source,
[
	source_node/1,
	source_node_property/2,
	create_source_node/2,
	delete_source_node/1,
	add_source_node_property/2,
	delete_source_node_property/2
]).

:- dynamic parsed_node_attr/2.
:- dynamic parsed_node_id/1.

create_source_node(Type,Id):-
    unused_id(Type,Id),
    my_assert(parsed_node_id(Id)).
    
delete_source_node(Id):-
	retractall(parsed_node_id(Id)),
	retractall(parsed_node_property(Id,_)).

add_source_node_property(Id,Prop):-
    my_assert(parsed_node_attr(Id,Prop)).

delete_source_node_property(Id,Prop):-
    retractall(parsed_node_attr(Id,Prop)).
    
source_node(source_folder_node(N)):-
    parsed_node_id(source_folder_node(N)).
source_node(compilation_unit_node(N)):-
    parsed_node_id(compilation_unit_node(N)).
source_node(atom_node(N)):-
    parsed_node_id(atom_node(N)).
source_node(variable_node(N)):-
    parsed_node_id(variable_node(N)).
source_node(string_node(N)):-
    parsed_node_id(string_node(N)).
source_node(brace_node(N)):-
    parsed_node_id(brace_node(N)).
source_node(list_node(N)):-
    parsed_node_id(list_node(N)).
source_node(compound_node(N)):-
    parsed_node_id(compound_node(N)).        
    

source_node_property(source_folder_node(Num),type(source_folder)):-
    source_node(source_folder_node(Num)).
source_node_property(compilation_unit_node(Num),type(compilation_unit)):-
    source_node(compilation_unit_node(Num)).
source_node_property(atom_node(Num),type(atom)):-
    source_node(atom_node(Num)).
source_node_property(variable_node(Num),type(variable)):-
    source_node(variable_node(Num)).
source_node_property(string_node(Num),type(string)):-
    source_node(string_node(Num)).
source_node_property(brace_node(Num),type(brace)):-
    source_node(brace_node(Num)).
source_node_property(list_node(Num),type(list)):-
    source_node(list_node(Num)).
source_node_property(compound_node(Num),type(compound)):-
    source_node(compound_node(Num)).    
source_node_property(Id,Prop):-
    parsed_node_attr(Id,Prop).       
source_node_property(compound_node(Num),arguments([])):-
    source_node(compound_node(Num)),
    \+ parsed_node_attr(compound_node(Num),arguments(_)).       
source_node_property(list_node(Num),elements([])):-
    source_node(list_node(Num)),
    \+ parsed_node_attr(list_node(Num),elements(_)).       
    
    
my_assert(parsed_node_id(Id)):-
    %format("assert(~w)~n",[Term]),
    ( 	parsed_node_id(Id)
	;   assert(parsed_node_id(Id))
	). 

my_assert(parsed_node_attr(Id,Attr)):-
    %format("assert(~w)~n",[Term]),
    my_assert(parsed_node_id(Id)),
    (	parsed_node_attr(Id,Attr)
    ;	assert(parsed_node_attr(Id,Attr))
    ).

unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( parsed_node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	
     