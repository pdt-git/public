%% 
% <module> Utillities for dealing with context terms
%
% This module is intended to make working with context terms more simple
:-module(pdt_util_context,[]).



%% pdt_define_context(+Template)
% create utility predicates for dealing with context terms of a form specified by Template.
%
% Lets say you put the following line into source file defining a module my_mode:
% =| :- pdt_define_context(foo(bar,baz,rumpel,knarz)). |=
%
% Then the following predicates will be created for you:
% 
% 	my_mode:foo_new(foo(_,_,_,_)).
% 	my_mode:foo_bar(foo(B,_,_,_),B).
% 	my_mode:foo_baz(foo(_,B,_,_),B).
% 	my_mode:foo_rumpel(foo(_,_,B,_),B).
% 	my_mode:foo_knarz(foo(_,_,_,B),B).
% 	my_mode:foo_set_bar(foo(_,A,B,C),Bar,(Bar,A,B,C)).
% 	my_mode:foo_set_baz(foo(A,_,B,C),Baz,(A,Baz,B,C)).
% 	my_mode:foo_set_rumpel(foo(A,B,_,C),Rumpel,(A,B,Rumpel,C)).
% 	my_mode:foo_set_knarz(foo(A,B,C,_),Knarz,(Bar,A,B,C)).
% 
% 	my_mode:foo_get(Foo,FieldValueList):-
%   	pdt_util_context:pdt_context_get_values(my_mode,Foo,FieldValueList).
% 
% 	my_mode:foo_set(FooIn,FieldValueList,FooOut):-
%   	pdt_util_context:pdt_context_set_values(my_mode,FooIn,FieldValueList,FooOut).
% 
% (please refer to the documentation of pdt_context_set_values/4 and pdt_context_get_values/3)
%
% @param Template should be a compound term whose arguments are all atoms.
%        The principal functor name will be used as the name of that context type. The arguments
% 		 will be used as field names. Note: The name has to be unique within the defining module.
%		 That is, you cannot define contexts foo(a,b) and foo(a,b,c) in the same module.



:- multifile '$context_template'/3.

user:term_expansion((:-pdt_define_context(Term)),Decls):-
    prolog_load_context(module,Module), 
    define_context(Module,Term,Decls).
user:term_expansion((:-pdt_export_context(Name)),Decls):-
	prolog_load_context(module,Module), 
    export_context(Module,Name,Decls).
    


%% pdt_define_context(+Module, +Template)
% Same as pdt_define_context/1 but asserts the predicates in the specified module rather than the calling
% module.
define_context(Module,Term,[pdt_util_context:'$context_template'(Module,Name,Term),MultiSetter,MultiGetter,Constructor|Decls]):-
	functor(Term,Name,Arity),    
    findall(
    	Decl,
    	(	arg(N,Term,ArgName),
    		(	create_getter(Name,Arity,N,ArgName,Decl)
    		;	create_setter(Name,Arity,N,ArgName,Decl)
    		)
    	),
    	Decls
    ),    
    create_multi_setter(Module,Term,MultiSetter),
    create_multi_getter(Module,Term,MultiGetter),
    create_constructor(Name,Arity,Constructor).
    

export_context(Module,Name,Decls):-
    '$context_template'(Module,Name,Term),
    findall(Decl,export_decl(Term,Decl),Decls).


export_decl(Term,( :- export(Head))):-
    functor(Term,Name,_),
    arg(_,Term,ArgName),
    concat_atom([Name,'_',ArgName],GName),
    functor(Head,GName,2).    
export_decl(Term,( :- export(Head))):-
    functor(Term,Name,_),
    arg(_,Term,ArgName),
    concat_atom([Name,'_set_',ArgName],SName),
    functor(Head,SName,3).    
export_decl(Term,( :- export(Head))):-
    functor(Term,Name,_),
    atom_concat(Name,'_get',GName),
    functor(Head,GName,2).
export_decl(Term,( :- export(Head))):-
    functor(Term,Name,_),
    atom_concat(Name,'_set',SName),
    functor(Head,SName,3).
export_decl(Term,( :- export(Head))):-
    functor(Term,Name,_),
    atom_concat(Name,'_new',CName),
    functor(Head,CName,1).
	
    
create_constructor(Name,Arity,Constructor):-
	atom_concat(Name,'_new',CName),
	functor(Template,Name,Arity),
	functor(Constructor,CName,1),
	arg(1,Constructor,Template).
	
create_getter(Name,Arity,N,ArgName,Getter):-
	concat_atom([Name,'_',ArgName],GName),
	functor(Template,Name,Arity),
	arg(N,Template,Value),
	functor(Getter,GName,2),
	arg(1,Getter,Template),
	arg(2,Getter,Value).
	
create_setter(Name,Arity,N,ArgName,Setter):-
	concat_atom([Name,'_set_',ArgName],SName),	
	functor(InTemplate,Name,Arity),	
	functor(OutTemplate,Name,Arity),
	arg(N,OutTemplate,Value),
	bind_other_args(N,InTemplate,OutTemplate,1,Arity),
	functor(Setter,SName,3),
	arg(1,Setter,InTemplate),
	arg(2,Setter,Value),
	arg(3,Setter,OutTemplate).
	
bind_other_args(N,InTemplate,OutTemplate,N,Arity):-
    !,
    I is N + 1,
    bind_other_args(N,InTemplate,OutTemplate,I,Arity).
bind_other_args(_N,_InTemplate,_OutTemplate,I,Arity):-
    I > Arity,
    !.    
bind_other_args(N,InTemplate,OutTemplate,I,Arity):-
    arg(I,InTemplate,Value),
    arg(I,OutTemplate,Value),
	J is I + 1,
    bind_other_args(N,InTemplate,OutTemplate,J,Arity).  
    
create_multi_getter(Module,Term,MultiGetter):-
    functor(Term,Name,_Arity),  
    atom_concat(Name,'_get',GName),
    functor(Head,GName,2),
    arg(1,Head,Context),
    arg(2,Head,ArgNameValues),
    MultiGetter=':-'(Head,pdt_util_context:pdt_context_get_values(Module,Context,ArgNameValues)).

create_multi_setter(Module,Term,MultiSetter):-
    functor(Term,Name,_Arity),  
    atom_concat(Name,'_set',GName),
    functor(Head,GName,3),
    arg(1,Head,InContext),
    arg(2,Head,ArgNameValues),
    arg(3,Head,OutContext),
    MultiSetter=':-'(Head,pdt_util_context:pdt_context_set_values(Module,InContext,ArgNameValues,OutContext)).

%% pdt_context_get_values(+Module, +Context, NameValuePairs)
% convenience method to access several fields of a context at the same time.
% Typically, the module defining the context will use the =|<context name>_get/2|= predicate instead.
%
% @param Module the module defining the context.
% @param Context the context term
% @param NameValuePairs a list of terms of the form =|name=value|=.
%        For each element, =value= will be unified with the value of the context field =name=.
pdt_context_get_values(Module,Template,NameValuePairs):-
    context_get_values(NameValuePairs,Module,Template).

context_get_values([],_Module,_Template).
context_get_values([ArgName=Value|ArgNameValues],Module,Template):-
	 pdt_context_get_value(Module,Template,ArgName,Value),
	 context_get_values(ArgNameValues,Module,Template).


pdt_context_get_value_alternative(Module,Template,ArgName,Value):-
    functor(Template,Name,_),
    (	var(ArgName) -> pdt_builder:debugme ; true),
    concat_atom([Name,'_',ArgName],GName),
	functor(Getter,GName,2),
	arg(1,Getter,Template),
	arg(2,Getter,Value),
	Module:Getter.
pdt_context_get_value(Module,CxTerm,ArgName,Value):-
    functor(CxTerm,Name,_),
	'$context_template'(Module,Name,Tmpl),
	arg(I,Tmpl,ArgName),
	!,
	arg(I,CxTerm,Value).




%%pdt_context_set_values(+Module, +Context, +NameValuePairs)
% convenience method to replace several field values of a context at the same time.
% The original Context term will not be modified.
%
% Typically, the module defining the context will use the =|<context name>_set/3|= predicate instead.
%
% @param Module the module defining the context.
% @param Context the context term
% @param NameValuePairs a list of terms of the form =|name=value|=.
%        For each element, =value= will replace the the context field =name=.
% @param NewContext the new context term resulting from the replacements.
pdt_context_set_values(_Module,Template,[],Template):-
	!.
pdt_context_set_values(Module,InTemplate,[ArgName=Value|ArgNameValues],OutTemplate):-
    pdt_context_set_value(Module,InTemplate,ArgName,Value,NextTemplate),
    pdt_context_set_values(Module,NextTemplate,ArgNameValues,OutTemplate).
    
pdt_context_set_value(Module,InTemplate,ArgName,Value,OutTemplate):-
	functor(InTemplate,Name,_),	
    (	var(ArgName) -> debugme ; true),	
    concat_atom([Name,'_set_',ArgName],SName),	
	functor(Setter,SName,3),
	arg(1,Setter,InTemplate),
	arg(2,Setter,Value),
	arg(3,Setter,OutTemplate),
	Module:Setter.	