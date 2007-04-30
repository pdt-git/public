:- module(pef_base,[]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	


define_assert(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_assert',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,assert(Cx))),
    export(Head).

define_query(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,call(Cx))),
    export(Head).

define_retractall(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_retractall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,retractall(Cx))),
    export(Head).




%%
% define_pef(+Template).
%
% define a new PEF type.
% Suppose template is foo(bar,baz).
% Then this call will generate and export predicates foo_assert(+List), foo_retractall(+List)
% and foo_query(+List), where List is a list of key=Value pairs.
% E.g. you can use foo_query([bar=bang,baz=V]), for retreiveing the baz of all foos with 
% a bar of bang.
% @param Template should be a ground compound term like in pdt_define_context/1.

define_pef(Template):-
	pdt_define_context(Template),
    define_assert(Template),
    define_retractall(Template),
    define_query(Template).

:- define_pef(pef_module_definition(id,name,file_ref,toplevel_ref)).

:- define_pef(pef_file_dependency(id,file_ref,dep_ref,toplevel_ref)).

:- define_pef(pef_property(id,key,value)).

:- define_pef(pef_problem(id,severity,file_ref,start,end,type,data)).