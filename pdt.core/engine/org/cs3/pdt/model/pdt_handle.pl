/* $LICENSE_MSG$(ld) */

:- module(pdt_handle,[
	pdt_property/3,
	pdt_lookup/3,
	pdt_virtual_handle/3,
	pdt_property/4,	
	pdt_properties/2,
	pdt_property_cached/3,
	pdt_add_property_factory/2,
	pdt_remove_property_factory/2
]).
:-dynamic factory/2.

pdt_virtual_handle(Type,Props,virtual_handle(Props,handle(_Id,Type,_Cache))).

pdt_add_property_factory(Type,Factory):-
    factory(Type,Factory),!.
pdt_add_property_factory(Type,Factory):-
    assert(factory(Type,Factory)).

pdt_remove_property_factory(Type,Factory):-
	retractall(factory(Type,Factory)).

pdt_property(virtual_handle(_,handle(Id,Type,Cache)),Name,Value):-    
	nonvar(Id),
	!,
	pdt_property(handle(Id,Type,Cache),Name,Value).
pdt_property(virtual_handle(Props,Handle),Name,Value):-    	
	handle(_,Type,_)=Handle,
	pdt_lookup(Props,Type,Handle),
	pdt_property(Handle,Name,Value).
pdt_property(handle(Id,Type,Cache),Name,Value):-    
    var(Cache),
    !,
    pdt_lookup(handle(Id,Type,Cache)),
    pdt_property(handle(Id,Type,Cache),Name,Value).
pdt_property(handle(_,_,Cache),Name,Value):-    
    pdt_property_cached(handle(_,_,Cache),Name,Value).
pdt_property(handle(Id,Type,Cache),Name,Value):-
    factory(Type,Factory),
    Factory:get_property(handle(Id,Type,Cache),Name,Value).

pdt_property_cached(handle(_,_,Cache),Name,Value):-
    member(Prop,Cache),
    property_name_value(Prop,Name,Value).

pdt_lookup(Props,Type,H):-
    factory(Type,Factory),
    Factory:lookup_handle(Props,H),
    handle(Id,_,Cache)=H,
    nonvar(Id),
    nonvar(Cache).


property_name_value(Name,Name,true):-
    atom(Name),
    !.
property_name_value(Prop,Name,Value):-
    functor(Prop,Name,1),
    !,
    Prop=..[Name,Value].
property_name_value(Prop,Name,Value):-
	Prop=..[Name,Args],
	Value=..[array,Args].    
	
pdt_properties(_,[]).
pdt_properties(H,[Prop|Props]):-
    Prop=..[Name,Value],
    pdt_property(H,Name,Value,false),
    pdt_properties(H,Props).

pdt_property(H,Name,Value,_):-
	pdt_property(H,Name,Value),
	!.
pdt_property(_,_,Default,Default).	

