:-use_module(library('/org/cs3/pdt/model/pdt_index')).
:-use_module(library('/org/cs3/pdt/model/pdt_handle')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_multimap')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_set')).
:-use_module(library('/org/cs3/pdt/core/pdt_meta_info')).

:-use_module(library(prolog_xref)).
visible_in_context(ContextModule,H):-
    pdt_property(H,module,ContextModule),
    !.
visible_in_context(_,H):-	
	pdt_property(H,module,user),
	!.
visible_in_context(_,H):-	
	pdt_property(H,module,system),
	!.
visible_in_context(_,H):-	
	pdt_property(H,exported,true),
	!.	
resolve(ContextModule,Head,Module:Name/Arity):-

	functor(Head,Name,Arity),
    pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,arity,Arity),
    pdt_property(H,file,DefFile),
    (	ContextModule=user
    ->	ContextFile=DefFile
    ;   pdt_file_module(ContextFile,ContextModule)
    ),
    pdt_file_depends(ContextFile,DefFile),
    !,
    pdt_property(H,module,Module).
resolve(_ContextModule,Head,Module:Name/Arity):-
	functor(Head,Name,Arity),
    pdt_index_load(builtin_predicates,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,arity,Arity),
    !,
    pdt_property(H,module,Module).
resolve(_ContextModule,Head,'$unresolved':Name/Arity):-
	functor(Head,Name,Arity).
strip_context(_ContextModule,RealContextModule:RealHead,RealContextModule,RealHead):-
    !.
strip_context(ContextModule,Head,ContextModule,Head).

type_name(Module:Name/Arity,ArgNum,type(Module:Name/Arity,ArgNum)).

print_type_bindings(Cx,CID,Term):-
    type_bindings(Cx,CID,Term,Map),
    system_types(Map,SysTypes),
    concrete_types(Map,ConcreteTypes),
    abstract_types(Map,AbstractTypes),
    forall(pdt_set_element(SysTypes,Type),print_system_node(Type)),

    forall(pdt_set_element(ConcreteTypes,Type),print_concrete_node(Type)),   
     forall(pdt_set_element(AbstractTypes,Type),print_abstract_node(Type)),   
    forall(pdt_multimap_get(Map,Type,Binding),print_type_binding(Term,Type,Binding)).

print_type_binding(Term,Type,Binding):-
    copy_term(c(Term,Type,Binding),c(TermCopy,TypeCopy,BindingCopy)),
    numbervars(TermCopy,0,_,[attvar(bind)]),
    format("\"~p\" -> \"~p\"~n",[TypeCopy,BindingCopy]),
    print_component_bindings(Term,Binding).
    


print_component_bindings(Clause,Term):-
    term_variables(Term,Vars),
    print_component_bindings2(Clause,Term,Vars).


print_component_bindings2(_Clause,_Term,[]).
print_component_bindings2(Clause,Term,[Var|Vars]):-
    (	attvar(Var)    
    ->  context_module(Module),
    	get_attr(Var,Module,Types),
    	copy_term(c(Clause,Term,Var,Types),c(ClauseCopy,TermCopy,VarCopy,TypesCopy)),
    	numbervars(c(ClauseCopy,TermCopy,VarCopy,TypesCopy),0,_,[attvar(bind)]),
    	print_var_bindings(TermCopy,VarCopy,TypesCopy)
    ;	true
    ),
    print_component_bindings2(Clause,Term,Vars).


print_var_bindings(_Term,_Var,[]).
print_var_bindings(Term,Var,[Type|Types]):-
	format("\"~p\" -> \"~p\" [label=~p, style=dotted]~n",[Term,Type,Var]),
	print_var_bindings(Term,Var,Types).
	
print_concrete_node(Node):-
    copy_term(Node,Copy),
    numbervars(Copy,0,_,[attvar(bind)]),
    format("\"~p\" [shape=rect]~n",[Copy]).

print_system_node(Node):-
    copy_term(Node,Copy),
    numbervars(Copy,0,_,[attvar(bind)]),
    format("\"~p\" [shape=diamond,style=filled,fillcolor=red]~n",[Copy]).

print_abstract_node(Node):-
    copy_term(Node,Copy),
    numbervars(Copy,0,_,[attvar(bind)]),
    format("\"~p\" [shape=ellipse]~n",[Copy]).        

type_bindings(CxModule,CID,Term,Map1):-
	pdt_multimap_empty(Map0),
	type_bindings(CxModule,Map0,CID,Term,Map1).

type_bindings(CxModule,Map0,CID,Term,Map1):-   
    strip_context(CxModule,Term,RealCxModule,RealHead),
%    resolve(RealCxModule,RealHead,Module:Name/Arity),
    type_bindings2(RealCxModule,Map0,CID,RealHead,Map1).

type_bindings2(_CxModule,Map,_CID,Var,Map):-   
    var(Var),
    !.
type_bindings2(CxModule,Map0,CID,Head:-Body,Map2):-
    !,
    type_bindings(CxModule,Map0,CID,Head,Map1),
    type_bindings(CxModule,Map1,CID,Body,Map2).
type_bindings2(CxModule,Map0,CID,:-Body,Map1):-
    !,
    type_bindings(CxModule,Map0,CID,Body,Map1).
type_bindings2(CxModule,Map0,CID,Term,Map2):-
    xref_meta(Term,Terms),
    !,
    %literal_type_bindings(CxModule,Map0,Term,Map1),
    metacall_type_bindings(CxModule,Map0,CID,Terms,Map2).
type_bindings2(CxModule,Map0,CID,Term,Map1):-
    literal_type_bindings(CxModule,Map0,CID,Term,Map1).
    
metacall_type_bindings(_CxModule,Map,_CID,[],Map).
metacall_type_bindings(CxModule,Map0,CID,[Term|Terms],Map2):-
    type_bindings(CxModule,Map0,CID,Term,Map1),
    metacall_type_bindings(CxModule,Map1,CID,Terms,Map2).

literal_type_bindings(CxModule,Map0,CID,Term,Map1):-
	resolve(CxModule,Term,Module:Name/Arity),
    arg_type_bindings(CxModule,Map0,CID,Term,1,Module:Name/Arity,Map1).
    
arg_type_bindings(_CxModule,Map,_CID,_Term,N,_Module:_Name/Arity,Map):-
    N>Arity,
    !.
arg_type_bindings(CxModule,Map0,CID,Term,N,Module:Name/Arity,Map2):-    
    type_name(Module:Name/Arity,N,TypeName),
    arg(N,Term,Arg),
    (	var(Arg)
    ->	add_type_to_var(Arg,TypeName)    	
    ;	true
    ),pdt_multimap_add(Map0,TypeName,var(CID,Arg),Map1),
    
    M is N+1,
    arg_type_bindings(CxModule,Map1,CID,Term,M,Module:Name/Arity,Map2).


attr_unify_hook(_Attr,Var):-
    nonvar(Var),
    !.
attr_unify_hook(Attr,Var):-
	 (	attvar(Var)
    ->	context_module(Module),
    	get_attr(Arg,Module,NewAttr),
    	append(Attr,NewAttr,OutAttr),
    	put_attr(Arg,Module,OutAttr)
	;   put_attr(Arg,Module,Attr) 	
    ).

add_type_to_var(Arg,TypeName):-
    context_module(Module),
    (	attvar(Arg)
    ->	get_attr(Arg,Module,Attr),
    	put_attr(Arg,Module,[TypeName|Attr])
	;   put_attr(Arg,Module,[TypeName]) 	
    ).

system_types(Bindings,Types):-
    pdt_set_findall(Elm,
    	(	pdt_multimap_get(Bindings,Key,Value),
    		(Elm=Key;Elm=Value),
 			type_name(system:_/_,_,Elm)
    	),
    	Types
    ).
concrete_types(Bindings,Types):-
    pdt_set_findall(Elm,
    	(	pdt_multimap_get(Bindings,Key,Value),
    		(Elm=Key;Elm=Value),
 			\+ functor(Elm,type,2)
    	),
    	Types
	).
abstract_types(Bindings,Types):-
    pdt_set_findall(Elm,
    	(	pdt_multimap_get(Bindings,Key,Value),
    		(Elm=Key;Elm=Value),
 			functor(Elm,type,2)
    	),
    	Types
	).

:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
typetest(File):-
	
    forall(	pdt_file_term(File,T),
    	(	source_term_expand(T,TT),
%    		portray_clause(TT),
			source_term_property(T,n,CID),
    		print_type_bindings(user,CID,TT)
    	)
    ).    
    
