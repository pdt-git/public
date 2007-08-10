:- module(pef_base,
	[	pef_reserve_id/2, 
		pef_type/2,
		pef_start_recording/1,
		pef_stop_recording/0,
		pef_clear_record/1,
		pef_count/2
	]
).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	
:- dynamic pef_pred/2.
:- dynamic pef_type/2.
:- dynamic pef_edge/5.
:- dynamic '$metapef_concrete'/1.
:- dynamic '$metapef_template'/2.
:- dynamic '$metapef_edge'/3.
:- dynamic '$metapef_is_a'/2.
:- dynamic '$metapef_attribute_tag'/3.
:- dynamic '$metapef_type_tag'/2.
:- dynamic '$option'/1.

:- dynamic pef_before_assert_hook/2, pef_before_retract_hook/2, pef_after_assert_hook/2, pef_after_retract_hook/2.
:- multifile pef_before_assert_hook/2, pef_before_retract_hook/2, pef_after_assert_hook/2, pef_after_retract_hook/2.


:- thread_local '$recording'/1.
:- dynamic '$recording'/1.
:- dynamic '$record_key'/2.



%% pef_count(+Type,-Count)
% true if count is the number of pefs of type Type.
pef_count(Type,Count):-
    findall(CType,
    	(    metapef_is_a(CType,Type),
    		'$metapef_concrete'(CType)
    	),
    	CTypes
    ),
    pef_count_X(CTypes,0,Count).

pef_count_X([],C,C).
pef_count_X([Type|Types],C1,C3):-
	'$metapef_template'(Type,Template),
	functor(Template,Type,Arity),
	functor(Head,Type,Arity),
	predicate_property(Head,number_of_clauses(Count)),
	C2 is C1 + Count,
	pef_count_X(Types,C2,C3).

pef_start_recording(Term):-
	record_key(Term,Key),
	asserta('$recording'(Key)).
	
pef_stop_recording:-
	retract('$recording'(_)),
	!.
pef_stop_recording:-
    throw(not_recording).

pef_clear_record(Term):-
    record_key(Term,Key),
    forall(
    	recorded(Key,CleanupGoal,RecordRef),
    	(	call(CleanupGoal),
    		erase(RecordRef)
    	)
    ).
record_key(Term,Key):-
    '$record_key'(Term,Key),
    !.
record_key(Term,Key):-
	pef_reserve_id('$record_key',Key),
	assert('$record_key'(Term,Key)).
% use @ to attach "tags" to attributes
% note that @ binds stronger than :, so you do not need parenthesis.
% you can "chain" several tags to one attribute.
:- op(550,xfy,@).


define_assert_old(Template):-
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
/*
define_assert(Template):-
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),
    atom_concat(Name,'_assert',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    create_index_asserts(Cx,Asserts,PefRef),
    (	'$option'(hooks)
    ->	(	find_id(Template,IdNum)
    	->	arg(IdNum,Cx,Id)
    	;	Id=PefRef
    	),
    	assert(
    		(	Head:-
    				Getter,
    				forall(pef_before_assert_hook(Id,Name),true),
    				Asserts,
    				forall(pef_after_assert_hook(Id,Name),true)
    		),Ref)
    ;   assert((Head:-Getter,Asserts),Ref)
    ),
    assert(pef_pred(Name,Ref)),
    export(Head).
*/
define_assert(Template):-
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
    create_index_asserts(Cx,Asserts,PefRef),
    
    (	'$metapef_type_tag'(Name,no_cleanup)
    ->	AssertsRecord = Asserts
    ;	create_record(Template,Cx,Record),
    	AssertsRecord = (Asserts,Record)
    ),
    (	'$option'(hooks)
    ->	(	find_id(Template,IdNum)
    	->	arg(IdNum,Cx,Id)
    	;	Id=PefRef
    	),
    	assert(
    		(	Head:-
    				Getter,
    				IdCheck,
    				forall(pef_before_assert_hook(Id,Name),true),
    				AssertsRecord,
    				forall(pef_after_assert_hook(Id,Name),true)
    		),Ref)
    ;   assert((Head:-Getter,IdCheck,AssertsRecord),Ref)
    ),
    assert(pef_pred(Name,Ref)),
    export(Head).

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
		->	recordz(Key,RetractGoal)
		;	true
		).
    

create_index_asserts(Cx,Asserts):-
    create_index_asserts(Cx,Asserts,_).
    
create_index_asserts(Cx,Asserts,Ref):-
    functor(Cx,Name,Arity),
    create_index_asserts_args(Arity,Name,Cx,Ref,assert(Cx,Ref),Asserts).

create_index_asserts_args(0,_Name,_Cx,_Ref,Asserts,Asserts).
create_index_asserts_args(I,Name,Cx,Ref,Asserts,AssertsOut):-
    '$metapef_attribute_tag'(Name,I,index),
    !,
    arg(I,Cx,Arg),
	index_name(Name,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
	J is I - 1,
	create_index_asserts_args(J,Name,Cx,Ref,(Asserts,assert(IndexClause)),AssertsOut).  
create_index_asserts_args(I,Name,Cx,Ref,Asserts,AssertsOut):-
    J is I - 1,
	create_index_asserts_args(J,Name,Cx,Ref,Asserts,AssertsOut).  
    


define_query_old(Template):-
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

define_query(Template):-
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
	create_index_query(Cx,Query),
    assert((Head:-Getter,Query),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

create_index_query(Cx,Query):-
    functor(Cx,Name,Arity),
    create_index_query_args(Arity,Name,Cx,call(Cx),Query).

create_index_query_args(0,_Name,_Cx,Query,Query).
create_index_query_args(I,Name,Cx,Query,QueryOut):-
    '$metapef_attribute_tag'(Name,I,index),
    !,
    arg(I,Cx,Arg),
	index_name(Name,I,IxName),
    IndexQuery=..[IxName,Arg,Ref],
	J is I - 1,
	create_index_query_args(J,Name,Cx,(nonvar(Arg)-> IndexQuery,clause(Cx,_,Ref) ; Query),QueryOut).  
create_index_query_args(I,Name,Cx,Query,QueryOut):-
    J is I - 1,
	create_index_query_args(J,Name,Cx,Query,QueryOut).  
	

index_name(Type,ArgNum,IxName):-
    integer(ArgNum),
    !,
    '$metapef_template'(Type,Tmpl),
    arg(ArgNum,Tmpl,ArgName),
    concat_atom([Type,revix,ArgName],'$',IxName).
index_name(Type,ArgName,IxName):-
    concat_atom([Type,revix,ArgName],'$',IxName).
    
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


/*
there are two versions of retractall:
 one for pefs that don't use reverse indexing at all
 and the other one for pefs that use them.
 
 The second one has one clause for each indexed attribute. The first
 attribute which is bound is used for index lookup.
*/
define_retractall(Template):-

    
    functor(Template,Name,_),

 	(	'$metapef_attribute_tag'(Name,_,index)
 	->	define_retractall_indexed(Template)
 	;	define_retractall_unindexed(Template)
 	).
 	
define_retractall_unindexed(Template):-
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
    		assert(
    			(	Head:-
    					Getter,
    					forall(
    						clause(Cx,_,CRef),
    						(	forall(pef_before_retract_hook(Id,Name),true),
    							erase(CRef),
    							forall(pef_after_retract_hook(Id,Name),true)
    						)
    					)
    			),Ref)
    	;	assert(
    			(	Head:-
    					Getter,
    					forall(
    						clause(Cx,_,Id),
    						(	forall(pef_before_retract_hook(Id,Name),true),
    							erase(Id),
    							forall(pef_after_retract_hook(Id,Name),true)
    						)
    					)
    			),Ref)
    	)
    ;	assert((Head:-Getter,retractall(Cx)),Ref)
    ),
    assert(pef_pred(Name,Ref)),
    export(Head).
define_retractall_indexed(Template):- 	
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),        
    atom_concat(Name,'_retractall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    create_retract(Cx,Retracts),
    assert((Head:-Getter,Retracts),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).



create_retract(Cx,Retract):-
    functor(Cx,Name,Arity),
    create_retract_action(Cx,Ref,Action),
    (	'$option'(hooks)
    ->	'$metapef_template'(Name,Tmpl),
    	(	find_id(Tmpl,Num),
    		arg(Num,Cx,Id)
    	;	Id=Ref
    	),
    	AAction = 
    		(	forall(pef_before_retract_hook(Id,Name),true),
    			Action,
    			forall(pef_after_retract_hook(Id,Name),true)
    		),
    	create_retract_args(Arity,Cx,Name,Ref,Action,forall(clause(Cx,_,Ref),AAction),Retract)
    ;	create_retract_args(Arity,Cx,Name,Ref,Action,forall(clause(Cx,_,Ref),Action),Retract)
    ).



create_retract_args(0,_Cx,_Name,_Ref,_Action,Retract,Retract).
create_retract_args(I,Cx,Name,Ref,Action,Retract,RetractOut):-
    '$metapef_attribute_tag'(Name,I,index),
    !,
    arg(I,Cx,Arg),
	index_name(Name,I,IxName),
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
  	create_retract_args(J,Cx,Name,Ref,Action,Next,RetractOut).
create_retract_args(I,Cx,Name,Ref,Action,Retract,RetractOut):-
   	J is I - 1,    	 
  	create_retract_args(J,Cx,Name,Ref,Action,Retract,RetractOut).

create_retract_action(Cx,Ref,Action):-
    functor(Cx,Name,Arity),
    create_retract_action_args(Arity,Name,Cx,Ref,erase(Ref),Action).

create_retract_action_args(0,_Name,_Cx,_Ref,Retracts,Retracts).
create_retract_action_args(I,Name,Cx,Ref,Retracts,RetractsOut):-
    '$metapef_attribute_tag'(Name,I,index),
    !,
    arg(I,Cx,Arg),
	index_name(Name,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
	J is I - 1,
	create_retract_action_args(J,Name,Cx,Ref, (retractall(IndexClause),Retracts),RetractsOut).  
create_retract_action_args(I,Name,Cx,Ref,Retracts,RetractsOut):-
    J is I - 1,
	create_retract_action_args(J,Name,Cx,Ref,Retracts,RetractsOut).  



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


process_indices(Name):-
    forall(
    	'$metapef_attribute_tag'(Name,Num,index),
		(	index_name(Name,Num,IxName),
			dynamic(IxName/2)
		)
	).

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

define_pef(TypedTemplate0):-
    (	'$option'(implicit_ids)
    ->	ensure_has_id(TypedTemplate0,TypedTemplate)
    ;	TypedTemplate=TypedTemplate0
    ),
    strip_types(TypedTemplate,Template),
    functor(Template,Name,Arity),
    undefine_pef(Name),
    assert('$metapef_concrete'(Name),Ref),
    assert(pef_pred(Name,Ref)),    
    assert('$metapef_template'(Name,Template),Ref2),
    assert(pef_pred(Name,Ref2)),        
    process_types(TypedTemplate,_),    
    process_indices(Name),
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


metapef_is_a(A,A).
metapef_is_a(A,B):-
    (	nonvar(A)
    ->	'$metapef_is_a'(A,Tmp),
    	metapef_is_a(Tmp,B)
    ;	'$metapef_is_a'(Tmp,B),
    	metapef_is_a(A,Tmp)
    ).
metapef_is_a(A,any):-
    '$metapef_concrete'(A).


process_types(Tmpl:T,Stripped):-
    !,
    functor(Tmpl,Name,_),
    assert('$metapef_is_a'(Name,T),Ref),
    assert(pef_pred(Name,Ref)),
    process_types(Tmpl,Stripped).
process_types(Tmpl @ Tags,Stripped):-
    !,
    functor(Tmpl,Name,_),
    add_type_tags(Tags,Name),
    process_types(Tmpl,Stripped).

process_types(Tmpl,Stripped):-
	functor(Tmpl,Name,Arity),
	functor(Stripped,Name,Arity),	
	process_types_args(1,Name,Arity,Tmpl,Stripped).


process_types_args(I,_Name,Arity,_Tmpl,_Stripped):-
    I>Arity,
    !.
process_types_args(I,Name,Arity,Tmpl,Stripped):-
	process_types_arg(I,Name,Arity,Tmpl,Stripped),
	J is I + 1,
	process_types_args(J,Name,Arity,Tmpl,Stripped).

process_types_arg(I,Name,Arity,Tmpl,Stripped):-
    arg(I,Tmpl,Arg),
    process_types_arg_X(Arg,I,Name,Arity,Tmpl,Stripped).

process_types_arg_X(Arg:ArgT,I,Name,Arity,Tmpl,Stripped):-
    !,
	assert('$metapef_edge'(Name,I,ArgT),Ref),
	assert(pef_pred(Name,Ref)),
	
	
	/* implicit inverse index on all foreign key columns */
	
	(	'$option'(index_foreign_keys),
		'$metapef_attribute_tag'(Name,I,index)
    ->	true
    ;	assert('$metapef_attribute_tag'(Name,I,index),Ref2),
		assert(pef_pred(Name,Ref2))
	),
	
	process_types_arg_X(Arg,I,Name,Arity,Tmpl,Stripped).
process_types_arg_X(Arg @ Tags,I,Name,Arity,Tmpl,Stripped):-
    !,
	add_attribute_tags(Tags,Name,I),
	process_types_arg_X(Arg,I,Name,Arity,Tmpl,Stripped).	
process_types_arg_X(Arg,I,_Name,_Arity,_Tmpl,Stripped):-
    arg(I,Stripped,Arg).

add_type_tags(Tag@Tags,Name):-
    !,
    assert('$metapef_type_tag'(Name,Tag),Ref),
	assert(pef_pred(Name,Ref)),    
	add_type_tags(Tags,Name).
add_type_tags(Tag,Name):-
    assert('$metapef_type_tag'(Name,Tag),Ref),
	assert(pef_pred(Name,Ref)).
    

add_attribute_tags(Tag@Tags,Name,Num):-
    !,
    (	'$metapef_attribute_tag'(Name,Num,Tag)
    ->	true
    ;	assert('$metapef_attribute_tag'(Name,Num,Tag),Ref),
		assert(pef_pred(Name,Ref))
	),
	add_attribute_tags(Tags,Name,Num).
add_attribute_tags(Tag,Name,Num):-
    (	'$metapef_attribute_tag'(Name,Num,Tag)
    ->	true
    ;	assert('$metapef_attribute_tag'(Name,Num,Tag),Ref),
		assert(pef_pred(Name,Ref))
	).

process_meta_edges:-
    forall('$metapef_edge'(FromT,ArgNum,ToT),process_meta_edge(FromT,ArgNum,ToT)).

process_inverse_meta_edges:-
    forall('$metapef_edge'(FromT,ArgNum,ToT),process_inverse_meta_edge(FromT,ArgNum,ToT)).

metapef_ref(Type,RefType,RefArg):-
    metapef_is_a(Type,TargetType),
    '$metapef_edge'(RefType,RefArg,TargetType).
metapef_cascade(Type,Arg,ArgType):-
    '$metapef_attribute_tag'(Type,Arg,cascade),
    '$metapef_edge'(Type,Arg,TargetType),
    metapef_is_a(ArgType,TargetType),
    '$metapef_concrete'(ArgType).

metapef_index_arg(Type,Arg):-
    '$metapef_attribute_tag'(Type,Arg,index).

pef_edge(From,FromT,ArgName,To,ToT):-
    (	var(From),nonvar(To)
    ->	'$pef_inverse_edge'(To,ToT,ArgName,From,FromT)
    ;	'$pef_edge'(From,FromT,ArgName,To,ToT)
    ).
pef_node(Id,Type,Labels):-
    '$pef_node'(Id,Type,Labels).
valid_target(ToT,To):-
    pef_type(To,ToT).
process_meta_edge(FromT,ArgNum,ToT):-
    '$metapef_template'(FromT,FromTemplate),
    functor(FromTemplate,_,Arity),
    functor(FromHead,FromT,Arity),
    arg(ArgNum,FromTemplate,ArgName),
    arg(ArgNum,FromHead,To),
    forall(
    	(	metapef_is_a(SubT,ToT),
    		'$metapef_concrete'(SubT)
    	),
		(	(	find_id(FromTemplate,IdNum)  
		    ->	arg(IdNum,FromHead,From),
		    	Clause=
		    		(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
		        			call(FromHead),
		        			pef_type(To,SubT)
		        	)
			;   Clause =
					(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
		        			clause(FromHead,_,From),
		        			pef_type(To,SubT)
		        	)
		    ),
		    assert(Clause,Ref),
		    assert(pef_pred(FromT,Ref))
		)
	).

process_inverse_meta_edge(FromT,ArgNum,ToT):-
    '$metapef_template'(FromT,FromTemplate),
    functor(FromTemplate,_,Arity),
    functor(FromHead,FromT,Arity),
    arg(ArgNum,FromTemplate,ArgName),
    arg(ArgNum,FromHead,To),
   	
   	% use reverse index, if available.
   	(	'$metapef_attribute_tag'(FromT,ArgNum,index)
   	->	index_name(FromT,ArgNum,IxName),
    	IndexQuery=..[IxName,To,FromRef],
    	Query=(IndexQuery,clause(FromHead,_,FromRef))
    ;	Query=clause(FromHead,_,FromRef)
    ),
    forall(
    	(	metapef_is_a(SubT,ToT),
    		'$metapef_concrete'(SubT)
    	),
		(	(	find_id(FromTemplate,IdNum)  
		    ->	arg(IdNum,FromHead,From)	    	
			;   From=FromRef
		    ),
		    Clause= (	'$pef_inverse_edge'(To,SubT,ArgName,From,FromT):-
		        			Query
		        	),
		    assert(Clause,Ref),
		    assert(pef_pred(FromT,Ref))
		)
	).



find_id(Tmpl,Num):-
    arg(Num,Tmpl,id),
    !.

%add rules for obtaining nodes and their labels
process_meta_nodes:-
    forall('$metapef_concrete'(Name),
    	process_meta_node(Name)
    ).


process_meta_node(Name):-
    '$metapef_template'(Name,Tmpl),
    functor(Tmpl,Name,Arity),
    functor(Head,Name,Arity),
    findall(ArgNum,'$metapef_attribute_tag'(Name,ArgNum,label), ArgNums),
    args(ArgNums,Head,Labels),
    (	find_id(Tmpl,IdNum)  
    ->	arg(IdNum,Head,Id),
    	Clause=
    		(	'$pef_node'(Id,Name,Labels):-
        			call(Head)
        	)
	;   Clause =
			(	'$pef_node'(Id,Name,Labels):-
        			clause(Head,_,Ref),
        			Id=Ref
        	)
    ),
    assert(Clause,Ref),
    assert(pef_pred(Name,Ref)).
	

    
args([],_,[]).
args([Num|Nums],Term,[Arg|Args]):-
    arg(Num,Term,Arg),
    args(Nums,Term,Args).
    
    

undefine_pef(Name):-
    forall(pef_pred(Name,Ref),erase(Ref)),
    retractall(pef_pred(Name,_)).

pef_reserve_id(Type,Id):-
    flag(pef_next_id,Id,Id + 1),
    assert(pef_type(Id,Type)). 


:- ensure_loaded(pef_generic).

create_cleanup_preds:-
    forall('$metapef_concrete'(Type), create_cleanupall(Type)).

create_cleanupall(Type):-
    '$metapef_template'(Type,Template),
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),        
    atom_concat(Name,'_cleanupall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    Clause=(Head:-Getter,cleanall(Cx)),
    assert(Clause,Ref),
    assert(pefpred(Type,Ref)),
    export(Head).
    

    
	   
postprocess_pefs:-
   process_meta_edges,
   process_inverse_meta_edges,
   process_meta_nodes,
   create_cleanup_preds.

%'$option'(implicit_ids).

%:- define_pef(
%	pef_program(
%		id,
%		file:pef_file
%	)
%).

    
:- include(pef_definitions).


    
% IMPORTANT: the following line should stay at the end of the file. 
% They trigger the post-processing of the meta pefs that require a global perspective.

:- postprocess_pefs.