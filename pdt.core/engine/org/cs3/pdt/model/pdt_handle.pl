:- module(pdt_handle,[
	pdt_property/3,
	pdt_property_cached/3,
	pdt_add_property_factory/2,
	pdt_remove_property_factory/2
]).
:-dynamic factory/2.



pdt_add_property_factory(Type,Factory):-
    factory(Type,Factory),!.
pdt_add_property_factory(Type,Factory):-
    assert(factory(Type,Factory)).

pdt_remove_property_factory(Type,Factory):-
	retractall(factory(Type,Factory)).

pdt_property(handle(_,_,Cache),Name,Value):-    
    pdt_property_cached(handle(_,_,Cache),Name,Value).
pdt_property(handle(Id,Type,Cache),Name,Value):-
    factory(Type,Factory),
    Factory:get_property(handle(Id,Type,Cache),Name,Value).

pdt_property_cached(handle(_,_,Cache),Name,Value):-
    member(Prop,Cache),
    property_name_value(Prop,Name,Value).

property_name_value(Name,Name,true):-
    atom(Name).
property_name_value(Prop,Name,Value):-
    functor(Prop,Name,1),
    !,
    Prop=..[Name,Value].
property_name_value(Prop,Name,Value):-
	Prop=..[Name,Args],
	Value=..[array,Args].    
	
