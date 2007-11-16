:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).





decls_query(Var,_Query):-
    var(Var),!,fail.
decls_query([A|_],Query):-
    copy_term(A,Query).
decls_query([_|Decls],Query):-
    decls_query(Decls,Query).
    
    
define_assert(Template,Decls):-
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),
    atom_concat(Name,'_assert',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    create_id_check(Template,Cx,IdCheck),
    create_index_asserts(Template,Cx,Decls,Asserts,PefRef),
    
    (	decls_query(Decls,'$metapef_type_tag'(Name,no_cleanup))
    ->	AssertsRecord = Asserts
    ;	create_record(Template,Cx,Record),
    	AssertsRecord = (Asserts,Record)
    ),
    (	'$option'(hooks)
    ->	(	find_id(Template,IdNum)
    	->	arg(IdNum,Cx,Id)
    	;	Id=PefRef
    	),
    	Decl=
    		(	Head:-
    				Getter,
    				IdCheck,
    				forall(pef_before_assert_hook(Id,Name),true),
    				AssertsRecord,
    				forall(pef_after_assert_hook(Id,Name),true)
    		)
    ;   Decl=(Head:-Getter,IdCheck,AssertsRecord)
    ),    
    memberchk(Decl,Decls),
    memberchk((:-export(Head)),Decls).

create_id_check(Template,Cx,IdCheck):-
    find_id(Template,IdNum),
    arg(IdNum,Cx,Id),
    functor(Template,Name,_),
    !,
    IdCheck=
		(	var(Id)
		->	pef_reserve_id(Name,Id)
		;	true
		).
create_id_check(_Template,_Cx,true).    

create_record(Template,Cx,Record):-
    find_id(Template,IdNum),
    arg(IdNum,Cx,Id),
    functor(Template,Name,_),
    atom_concat(Name,'_retractall',RetractName),
    RetractGoal=..[RetractName,[id=Id]],
    !,
    Record=
		(	'$recording'(Key)
		->	recorda(Key,RetractGoal)
		;	true
		).
    

    
create_index_asserts(Template,Cx,Decls,Asserts,Ref):-
    functor(Cx,Name,Arity),
    create_index_asserts_args(Arity,Name,Template,Cx,Decls,Ref,assert(Cx,Ref),Asserts).

create_index_asserts_args(0,_Name,_Template,_Cx,_Decls,_Ref,Asserts,Asserts):-!.
create_index_asserts_args(I,Name,Template,Cx,Decls,Ref,Asserts,AssertsOut):-
    decls_query(Decls,'$metapef_attribute_tag'(Name,I,index)),
    !,
    arg(I,Cx,Arg),
	index_name(Template,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
	J is I - 1,
	create_index_asserts_args(J,Name,Template,Cx,Decls,Ref,(Asserts,assert(IndexClause)),AssertsOut).  
create_index_asserts_args(I,Name,Template,Cx,Decls,Ref,Asserts,AssertsOut):-
    J is I - 1,
	create_index_asserts_args(J,Name,Template,Cx,Decls,Ref,Asserts,AssertsOut).  
    



define_query(Template,Decls):-
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
	create_index_query(Template,Cx,Decls,Query),
    memberchk((Head:-Getter,Query),Decls),
    memberchk((:- export(Head)),Decls).

create_index_query(Template,Cx,Decls,Query):-
    functor(Cx,Name,Arity),
    create_index_query_args(Arity,Name,Template,Cx,Decls,call(Cx),Query).

create_index_query_args(0,_Name,_Template,_Cx,_Decls,Query,Query):-!.
create_index_query_args(I,Name,Template,Cx,Decls,Query,QueryOut):-
    decls_query(Decls,'$metapef_attribute_tag'(Name,I,index)),
    !,
    arg(I,Cx,Arg),
	index_name(Template,I,IxName),
    IndexQuery=..[IxName,Arg,Ref],
	J is I - 1,
	create_index_query_args(J,Name,Template,Cx,Decls,(nonvar(Arg)-> IndexQuery,clause(Cx,_,Ref) ; Query),QueryOut).  
create_index_query_args(I,Name,Template,Cx,Decls,Query,QueryOut):-
    J is I - 1,
	create_index_query_args(J,Name,Template,Cx,Decls,Query,QueryOut).  
	

index_name(Tmpl,ArgNum,IxName):-    
    integer(ArgNum),
    !,    
    functor(Tmpl,Type,_),
    arg(ArgNum,Tmpl,ArgName),
    concat_atom([Type,revix,ArgName],'$',IxName).
index_name(Tmpl,ArgName,IxName):-
    functor(Tmpl,Type,_),
    concat_atom([Type,revix,ArgName],'$',IxName).
    
define_query2(Template,Decls):-
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
    memberchk((Head:-Constructor,Getter,call(Cx)),Decls),    
    memberchk((:- export(Head)),Decls).


/*
there are two versions of retractall:
 one for pefs that don't use reverse indexing at all
 and the other one for pefs that use them.
 
 The second one has one clause for each indexed attribute. The first
 attribute which is bound is used for index lookup.
*/
define_retractall(Template,Decls):-    
    functor(Template,Name,_),
 	(	decls_query(Decls,'$metapef_attribute_tag'(Name,_,index))
 	->	define_retractall_indexed(Template,Decls)
 	;	define_retractall_unindexed(Template,Decls)
 	).
 	
define_retractall_unindexed(Template,Decls):-
	functor(Template,Name,Arity), 	
    functor(Cx,Name,Arity),
    atom_concat(Name,'_retractall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    (	'$option'(hooks)
    ->	(	find_id(Template,IdNum)
    	->	arg(IdNum,Cx,Id),
    		Decl=
    			(	Head:-
    					Getter,
    					forall(
    						clause(Cx,_,CRef),
    						(	forall(pef_before_retract_hook(Id,Name),true),
    							erase(CRef),
    							forall(pef_after_retract_hook(Id,Name),true)
    						)
    					)
    			)
    	;	Decl=
    			(	Head:-
    					Getter,
    					forall(
    						clause(Cx,_,Id),
    						(	forall(pef_before_retract_hook(Id,Name),true),
    							erase(Id),
    							forall(pef_after_retract_hook(Id,Name),true)
    						)
    					)
    			)
    	)
    ;	Decl=(Head:-Getter,retractall(Cx))
    ),    
    memberchk(Decl,Decls),
    memberchk((:-export(Head)),Decls).

define_retractall_indexed(Template,Decls):- 	
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),        
    atom_concat(Name,'_retractall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    create_retract(Template,Cx,Decls,Retracts),
    Decl=(Head:-Getter,Retracts),    
    ExpDecl= (:- export(Head)),
    memberchk(Decl,Decls),
    memberchk(ExpDecl,Decls).



create_retract(Template,Cx,Decls,Retract):-
    functor(Cx,Name,Arity),
    create_retract_action(Template,Cx,Decls,Ref,Action),
    (	'$option'(hooks)
    ->	(	find_id(Cx,Num),
    		arg(Num,Cx,Id)
    	;	Id=Ref
    	),
    	AAction = 
    		(	forall(pef_before_retract_hook(Id,Name),true),
    			Action,
    			forall(pef_after_retract_hook(Id,Name),true)
    		),
    	create_retract_args(Arity,Template,Cx,Decls,Name,Ref,Action,forall(clause(Cx,_,Ref),AAction),Retract)
    ;	create_retract_args(Arity,Template,Cx,Decls,Name,Ref,Action,forall(clause(Cx,_,Ref),Action),Retract)
    ).



create_retract_args(0,_Template,_Cx,_Decls,_Name,_Ref,_Action,Retract,Retract):-!.
create_retract_args(I,Template,Cx,Decls,Name,Ref,Action,Retract,RetractOut):-
    decls_query(Decls,'$metapef_attribute_tag'(Name,I,index)),
    !,
    arg(I,Cx,Arg),
	index_name(Template,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
    Next=(	nonvar(Arg)
    	 ->	forall(
    	 		(	IndexClause, 
    	 			clause(Cx,_,Ref)
    	 		),
    	 		Action
    	 	)
    	 ;	Retract
    	 ),
	J is I - 1,    	 
  	create_retract_args(J,Template,Cx,Decls,Name,Ref,Action,Next,RetractOut).
create_retract_args(I,Template,Cx,Decls,Name,Ref,Action,Retract,RetractOut):-
   	J is I - 1,    	 
  	create_retract_args(J,Template,Cx,Decls,Name,Ref,Action,Retract,RetractOut).

create_retract_action(Template,Cx,Decls,Ref,Action):-
    functor(Cx,Name,Arity),
    create_retract_action_args(Arity,Name,Template,Cx,Decls,Ref,erase(Ref),Action).

create_retract_action_args(0,_Name,_Template,_Cx,_Decls,_Ref,Retracts,Retracts):-!.
create_retract_action_args(I,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut):-
    decls_query(Decls,'$metapef_attribute_tag'(Name,I,index)),
    !,
    arg(I,Cx,Arg),
	index_name(Template,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
	J is I - 1,
	create_retract_action_args(J,Name,Template,Cx,Decls,Ref, (retract(IndexClause),Retracts),RetractsOut).  
create_retract_action_args(I,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut):-
    J is I - 1,
	create_retract_action_args(J,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut).  




process_indices(Decls,_):-
    var(Decls),
    !.
process_indices(['$metapef_attribute_tag'(Name,Num,index)|Decls],Tmpl):-
    functor(Tmpl,Name,_),
    !,
    index_name(Tmpl,Num,IxName),
    memberchk((:- dynamic IxName/2),Decls),
    process_indices(Decls,Tmpl).
process_indices([_|Decls],Tmpl):-    
    process_indices(Decls,Tmpl).

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


user:term_expansion((:- define_pef(TypedTemplate0)),Decls):-    	
	pef_base:expand_definition(TypedTemplate0,Decls).

expand_definition(TypedTemplate0,Decls):-						  
    (	'$option'(implicit_ids)
    ->	ensure_has_id(TypedTemplate0,TypedTemplate)
    ;	TypedTemplate=TypedTemplate0
    ),
    strip_types(TypedTemplate,Template),
    functor(Template,Name,Arity),                    
    process_types(TypedTemplate,Decls),    
    process_indices(Decls,Template),
    memberchk((:-dynamic(Name/Arity)),Decls),
	define_assert(Template,Decls),
    define_retractall(Template,Decls),
    define_query(Template,Decls),
    define_query2(Template,Decls),
    expand_term((:-pdt_define_context(Template)),ContextDecls),	
	findall(Decl,pdt_util_context:export_decl(Template,Decl),ContextExportDecls),	
    append(ContextDecls,ContextExportDecls,TailDecls),
    has_tail(['$metapef_concrete'(Name), '$metapef_template'(Name,Template)|TailDecls],Decls).



ensure_has_id(In,Out):-
    strip_types(In,Stripped),
    (	find_id(Stripped,_)
    ->	Out=In
    ;	add_id(In,Out)    	
    ).

add_id(In:Type,Out:Type):-
    !,
    add_id(In,Out).
add_id(In @Tags,Out @Tags):-
    !,
    add_id(In,Out).
add_id(In,Out):-
	In =.. InX,
    append(InX,[id @index],OutX),
    Out =.. OutX.

% remove type identifier from a template
strip_types(T1:_,T2):-
    !,
    strip_types(T1,T2).
strip_types(T1 @ _,T2):-
    !,
    strip_types(T1,T2).
strip_types(T1,T2):-
    T1=..[F|Args1],
    strip_types_args(Args1,Args2),
    T2=..[F|Args2].

strip_types_args([],[]).
strip_types_args([Arg:_|Args1],[StrippedArg|Args2]):-
    !,
    strip_types(Arg,StrippedArg),
    strip_types_args(Args1,Args2).
strip_types_args([Arg @ _|Args1],[Arg|Args2]):-
    !,
    strip_types_args(Args1,Args2).
strip_types_args([Arg|Args1],[Arg|Args2]):-
    strip_types_args(Args1,Args2).

    
process_types(Tmpl:T,Decls):-
    !,
    functor(Tmpl,Name,_),
    memberchk('$metapef_is_a'(Name,T),Decls),
    process_types(Tmpl,Decls).
process_types(Tmpl @ Tags,Decls):-
    !,
    functor(Tmpl,Name,_),
    add_type_tags(Tags,Name,Decls),
    process_types(Tmpl,Decls).
process_types(Tmpl,Decls):-
	functor(Tmpl,Name,Arity),
	process_types_args(1,Name,Arity,Tmpl,Decls).


process_types_args(I,_Name,Arity,_Tmpl,_Decls):-
    I>Arity,
    !.
process_types_args(I,Name,Arity,Tmpl,Decls):-
	process_types_arg(I,Name,Arity,Tmpl,Decls),
	J is I + 1,
	process_types_args(J,Name,Arity,Tmpl,Decls).

process_types_arg(I,Name,Arity,Tmpl,Decls):-
    arg(I,Tmpl,Arg),
    process_types_arg_X(Arg,I,Name,Arity,Tmpl,Decls).

process_types_arg_X(Arg:ArgT,I,Name,Arity,Tmpl,Decls):-
    !,
	memberchk('$metapef_edge'(Name,I,ArgT),Decls),
	
	/* implicit inverse index on all foreign key columns */	
	(	'$option'(index_foreign_keys)	
    ->	memberchk('$metapef_attribute_tag'(Name,I,index),Decls)
    ;	true
	),	
	process_types_arg_X(Arg,I,Name,Arity,Tmpl,Decls).
process_types_arg_X(Arg @ Tags,I,Name,Arity,Tmpl,Decls):-
    !,
	add_attribute_tags(Tags,Name,I,Decls),
	process_types_arg_X(Arg,I,Name,Arity,Tmpl,Decls).	
process_types_arg_X(_Arg,_I,_Name,_Arity,_Tmpl,_Decls).

add_type_tags(Tag@Tags,Name,Decls):-
    !,    
    memberchk('$metapef_type_tag'(Name,Tag),Decls),	
	add_type_tags(Tags,Name,Decls).
add_type_tags(Tag,Name,Decls):-
    memberchk('$metapef_type_tag'(Name,Tag),Decls).
    

add_attribute_tags(Tag@Tags,Name,Num,Decls):-
    !,
    memberchk('$metapef_attribute_tag'(Name,Num,Tag),Decls),    
	add_attribute_tags(Tags,Name,Num,Decls).
add_attribute_tags(Tag,Name,Num,Decls):-
    memberchk('$metapef_attribute_tag'(Name,Num,Tag),Decls).
    
