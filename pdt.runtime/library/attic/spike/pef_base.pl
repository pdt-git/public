:- module(pef_base,[pef_reserve_id/2]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	
:- dynamic pef_pred/2.

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
    assert((Head:-Constructor,Getter,assert(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
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
    assert((Head:-Constructor,Getter,call(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

define_query2(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,2),
    arg(1,Head,List),
    arg(2,Head,Cx),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,call(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
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
    assert((Head:-Constructor,Getter,retractall(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

define_recorda(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recorda',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recorda(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),    
    export(Head).

define_recordz(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recordz',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recordz(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),
    export(Head).

define_recorded(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recorded',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recorded(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),
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
    functor(Template,Name,Arity),
    undefine_pef(Name),
    dynamic(Name/Arity),
	pdt_define_context(Template),	
	pdt_export_context(Name),
    define_assert(Template),
    define_retractall(Template),
    define_query(Template),
    define_query2(Template),
    define_recorded(Template),
    define_recorda(Template),
    define_recordz(Template).

undefine_pef(Name):-
    forall(pef_pred(Name,Ref),erase(Ref)),
    retractall(pef_pred(Name,_)).

pef_reserve_id(Type,Id):-
    flag(pef_next_id,Id,Id + 1),
    assert(pef_type(Id,Type)). 

:- define_pef(pef_module_definition(id,name,file_ref,toplevel_ref)).

:- define_pef(pef_op_definition(id,priority,type,name,file_ref,toplevel_ref)).

:- define_pef(pef_file_dependency(id,file_ref,dep_ref,toplevel_ref)).

:- define_pef(pef_property(id,key,value)).

:- define_pef(pef_problem(id,severity,file_ref,start,end,type,data)).

:- define_pef(pef_toplevel(file_ref,term,expanded,positions,varnames,singletons)).

:- define_pef(pef_term(id,name,arity)).

:- define_pef(pef_variable_occurance(id,variable_ref)).

:- define_pef(pef_variable(id,toplevel_ref)).

:- define_pef(pef_arg(num,parent,child)).

:- define_pef(pef_toplevel_root(root,toplevel_ref,file_ref)).

:- define_pef(pef_file(file_ref,toplevel_key)).