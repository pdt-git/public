/* $LICENSE_MSG$(ld) */

:-module(builtin_predicate_factory,[
	pdt_index_builtins/0
	]).



:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).



:- pdt_add_property_factory(builtin_predicate,builtin_predicate_factory).

lookup_handle(handle(id(Module:Name/Arity), builtin_predicate,Cache)):-
    pdt_index_load(builtin_predicates,Ix),
    pdt_index_get(Ix,Name,handle(id(Module:Name/Arity), builtin_predicate,Cache)).

get_property(_,_,_):-fail.

	
pdt_index_builtins:-
    forall(builtin(Module,Name,Arity),index_builtin(Module,Name,Arity)).
    
    
    
builtin(Module,Name,Arity):-    
	current_predicate(Name/Arity),
	functor(Head,Name,Arity),
	predicate_property(Head,built_in),
	user_imported_from(Head,Module).    
	
user_imported_from(Head,Module):-
	user:predicate_property(Head,imported_from(Module)),
	!.
user_imported_from(_,user).

index_builtin(Module,Name,Arity):-
    pdt_index_load(builtin_predicates,IX),
	index_builtin(Module,Name,Arity,IX,NextIX),
	pdt_index_store(builtin_predicates,NextIX).
	
index_builtin(Module,Name,Arity,IX,NextIX):-
	functor(Head,Name,Arity),
	findall(Prop,user:predicate_property(Head,Prop),Props),
	index_entry(Module:Name/Arity,[module(Module),name(Name),arity(Arity)|Props],Key,Value),
	pdt_index_put(IX,Key,Value,NextIX).
	
	
%index_entry(+Signature, +Props, -Key, -Value)
index_entry(Module:Name/Arity, Props,Name, handle(id(Module:Name/Arity), builtin_predicate,Props)).	

