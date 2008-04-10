:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).

:- op(600,xfy,@).



%-------------------------------------- 
% this section was backported from my last "spike".


/*
name is type name of the declared type
attributes is a term arg(arg decl, arg decl,...., arg decl) where arg decl is an attribute declaration term, see below.
supertypes is a list of type names.
tags is a list of tags

this data structure is for internal use in this module only.
*/
:- pdt_define_context(type_decl(name,attributes,supertypes,tags)).

/*
name is the name of the declared attribute.
types is a list of type names
tags is a list of tags

this data structure is for internal use in this module only.
*/
:- pdt_define_context(attr_decl(num,name,types,tags)).

%---------------------------------------
% the predicates in this section are used to parse
% type definition into a type_decl data structure.
% 
parse_type_declaration(Type @ TagAndMore,TypeDecl):-
    !,
    type_decl_new(TypeDecl),
    functor(Type,Name,_),
    type_decl_name(TypeDecl,Name),
    parse_tag_and_more(TagAndMore,Tags,SuperTypes),
    parse_attribute_declarations(Type,Attributes),
    type_decl_tags(TypeDecl,Tags),
    type_decl_supertypes(TypeDecl,SuperTypes),
    type_decl_attributes(TypeDecl,Attributes).
parse_type_declaration(Type : TypeAndMore,TypeDecl):-
    !,
    type_decl_new(TypeDecl),
    functor(Type,Name,_),
    type_decl_name(TypeDecl,Name),
    parse_type_and_more(TypeAndMore,Tags,SuperTypes),
    parse_attribute_declarations(Type,Attributes),
    type_decl_tags(TypeDecl,Tags),
    type_decl_supertypes(TypeDecl,SuperTypes),
    type_decl_attributes(TypeDecl,Attributes).
parse_type_declaration(Type,TypeDecl):-
    type_decl_new(TypeDecl),    
    functor(Type,Name,_),
    type_decl_name(TypeDecl,Name),
    parse_attribute_declarations(Type,Attributes),
    type_decl_tags(TypeDecl,[]),
    type_decl_supertypes(TypeDecl,[]),
    type_decl_attributes(TypeDecl,Attributes).

parse_tag_and_more(Tag @ TagAndMore,[Tag|Tags],Types):-
    !,
    parse_tag_and_more(TagAndMore,Tags,Types).
parse_tag_and_more(Tag : TypeAndMore,[Tag|Tags],Types):-
    !,
    parse_type_and_more(TypeAndMore,Tags,Types).
parse_tag_and_more(Tag,[Tag],[]).

parse_type_and_more(Type @ TagAndMore,Tags,[Type|Types]):-
    !,
    parse_tag_and_more(TagAndMore,Tags,Types).
parse_type_and_more(Type : TypeAndMore,Tags,[Type|Types]):-
    !,
    parse_type_and_more(TypeAndMore,Tags,Types).
parse_type_and_more(Type,[],[Type]).

parse_attribute_declarations(Type,Attributes):-
	functor(Type,_,Arity),
	functor(Attributes,attrs,Arity),
	parse_attribute_declarations(Arity,Type,Attributes).

parse_attribute_declarations(N,_Type,_Attributes):-    
    N==0,!.
parse_attribute_declarations(N,Type,Attributes):-
    arg(N,Type,TypeArg),
    arg(N,Attributes,AttributesArg),
    parse_attribute_declaration(N,TypeArg,AttributesArg),
    M is N - 1,
    parse_attribute_declarations(M,Type,Attributes).
    
parse_attribute_declaration(N,Arg,Decl):-
	attr_decl_new(Decl),
	attr_decl_num(Decl,N),
	(	Arg = Name @ TagAndMore
	->	parse_tag_and_more(TagAndMore,Tags,Types),
		attr_decl_name(Decl,Name),
		attr_decl_tags(Decl,Tags),
		attr_decl_types(Decl,Types)
	;	Arg = Name : TypeAndMore
	->	parse_type_and_more(TypeAndMore,Tags,Types),
		attr_decl_name(Decl,Name),
		attr_decl_tags(Decl,Tags),
		attr_decl_types(Decl,Types)
	;	attr_decl_name(Decl,Arg),
		attr_decl_tags(Decl,[]),
		attr_decl_types(Decl,[])
	).
	
%--------------------------------------	




%--------------------------------------------------------
% we should get rid of the predicates in this section.


decls_query(Var,_Query):-
    var(Var),!,fail.
decls_query([A|_],Query):-
    copy_term(A,Query).
decls_query([_|Decls],Query):-
    decls_query(Decls,Query).
    

%% generate an assert clause from a a type_decl data structure.
type_decl_assert_clause(TypeDecl,
	(	Head:-
			Getter,
			IdCheck,
			AssertAndRecord
	)
):-	type_decl_getter(TypeDecl,AttrList,Data,Getter),
    type_decl_name(TypeDecl,Name),
    atom_concat(Name,'_assert',HeadName),
    Head=..[HeadName,AttrList],
    type_decl_id_check(TypeDecl,Data,IdCheck),
    type_decl_assert_and_record(TypeDecl,Data,AssertAndRecord).
    
type_decl_assert_and_record(TypeDecl,Data,AssertAndRecord):-
	type_decl_index_assert(TypeDecl,Data,Assert),
	type_decl_tags(TypeDecl,Tags),
	(	memberchk(no_cleanup,Tags)
	->	AssertAndRecord=Assert
	;	type_decl_record(TypeDecl,Data,Record),
		AssertAndRecord=(Assert,Record)
	).    
    	
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

type_decl_id_check(TypeDecl,Data,IdCheck):-
    (	type_decl_attr_by_name(TypeDecl,id,AttrDecl)
    ->	attr_decl_num(AttrDecl,IdNum),
    	arg(IdNum,Data,Id),
    	type_decl_name(TypeDecl,Type),
    	IdCheck=(var(Id)->pef_reserve_id(Type,Id);true)
    ;	IdCheck=true
    ).
    
    
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

type_decl_record(TypeDecl,Data,Record):-
    type_decl_undo_assert_head(TypeDecl,Data,UndoGoal),
    Record=
    	(	'$recording'(Key,_)
    	->	asserta('$undo'(Key,UndoGoal))
    	;	true
    	).
    	
create_record(_Template,Cx,Record):-
    undo_assert_head(Cx,UndoGoal),
    !,
    Record=
		(	'$recording'(Key,_)
		->	recorda(Key,UndoGoal)
		;	true
		).
    

type_decl_revix_assert(TypeDecl,Data,RevIxAsserts):-
    (	bagof(RevIxAssert,type_decl_revix_assert(TypeDecl,Data,RevIxAssert),RevIxAsserts0)
    ->	list_conjunction(RevIxAsserts0,RevIxAsserts)
    ;	RevIxAsserts=true
    ).

list_conjunction([],true).
list_conjunction([A|List],(A,Conjunction)):-
    list_conjunction(List,Conjunction).

type_decl_revix_assert(TypeDecl,Data,RevIxAssert):-
    type_decl_revix_name(TypeDecl,RevIxAttrDecl,RevIxName),
    type_decl_attr_by_name(TypeDecl,id,IdAttrDecl),
    attr_decl_num(RevIxAttrDecl,RevIxNum),
    attr_decl_num(IdAttrDecl,IdNum),
    arg(IdNum,Data,Id),
    arg(RevIxNum,Data,Key),
    RevIxClause =.. [RevIxName,Key,Id],
    RevIxAssert = assert(RevIxClause).
 
 type_decl_cmpix_assert(TypeDecl,Data,Key,CmpixAssert):-
 	type_decl_cmpix_name(TypeDecl,CmpIxAttrDecls,CmpIxName),
 	type_decl_attr_by_name(TypeDecl,id,IdAttrDecl),
    attr_decl_num(IdAttrDecl,IdNum),
    arg(IdNum,Data,Id),
    CmpIxClause =.. [CmpIxName,Key,Id],
    CmpIxAssert = assert(CmpIxClause).
 
 
 type_decl_revix_name(TypeDecl,AttrDecl,RevIxName):-
    type_decl_name(TypeDecl,TypeName),
 	type_decl_attr_by_tag(TypeDecl,index,AttrDecl),
 	attr_decl_name(AttrDecl,AttrName),
 	concat_atom([TypeName,revix,AttrName],'$',RevIxName).   

type_decl_cmpix_name(TypeDecl,AttrNames,CmpIxName):-
    type_decl_tags(TypeDecl,Tags),
    type_decl_name(TypeDecl,TypeName),
    member(composite_index(AttrNames),Tags),
    concat_atom([TypeName,cmpix|AttrNames],'$',CmpIxName).


type_decl_cmpix_hashable(TypeDecl,AttrNames,Data,Hashable):-
    type_decl_tags(TypeDecl,Tags),
    member(composite_index(AttrNames),Tags),
    type_decl_attrs_by_names(TypeDecl,AttrNames,AttrDecls),
    concat_atom([TypeName,cmpix|AttrNames],'$',CmpIxName).

type_decl_attr_by_tag(TypeDecl,Tag,AttrDecl):-
    type_decl_attributes(TypeDecl,Attrs),
    arg(_,Attrs,AttrDecl),
    attr_decl_tags(AttrDecl,Tags),
    memberchk(Tag,Tags).

type_decl_attr_by_name(TypeDecl,Name,AttrDecl):-
    type_decl_attributes(TypeDecl,Attrs),
    arg(_,Attrs,AttrDecl),
    attr_decl_name(AttrDecl,Name).

type_decl_attr_by_type(TypeDecl,AttrType,AttrDecl):-
    type_decl_attributes(TypeDecl,Attrs),
    arg(_,Attrs,AttrDecl),
    attr_decl_types(AttrDecl,Types),
    memberchk(AttrType,Types).
    
 
    
create_index_asserts(Template,Cx,Decls,Asserts,Ref):-
    functor(Cx,Name,Arity),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->	cmpix_assert(Names,Template,Cx,Ref,CmpixAsserts),
    	Asserts0=(assert(Cx,Ref),CmpixAsserts)
    ;	Asserts0=assert(Cx,Ref)
    ),
    create_index_asserts_args(Arity,Name,Template,Cx,Decls,Ref,Asserts0,Asserts).
    

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

define_query2(Template,Decls):-
    functor(Template,Name,Arity),
    functor(Cx,Name,Arity),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,2),
    arg(1,Head,List),
    arg(2,Head,Cx),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
	create_index_query(Template,Cx,Decls,Query),
    memberchk((Head:-Getter,Query),Decls),
    memberchk((:- export(Head)),Decls).


create_index_query(Template,Cx,Decls,QueryOut):-
    functor(Cx,Name,Arity),
    create_index_query_args(Arity,Name,Template,Cx,Decls,call(Cx),Query),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->	cmpix_query(Names,Template,Cx,CmpixQuery),
    	QueryOut=(CmpixQuery;Query)
    ;	QueryOut=Query
    ).

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
index_name(Tmpl,[ArgName|ArgNames],IxName):-  
	!,  
    functor(Tmpl,Type,_),
    concat_atom([Type,cmpix|[ArgName|ArgNames]],'$',IxName).
index_name(Tmpl,ArgName,IxName):-    
    functor(Tmpl,Type,_),
    concat_atom([Type,revix,ArgName],'$',IxName).
    

cmpix_hashable(Names,Tmpl,Data,Hashable):-
    map_names_to_args(Names,Tmpl,Data,Args),
    Hashable=..[h|Args].

cmpix_query(Names,Tmpl,Data,Query):-
    index_name(Tmpl,Names,IxName),
    IxClause =..[IxName,Hash,Ref],
    cmpix_hashable(Names,Tmpl,Data,Hashable),
    Query=(ground(Hashable) -> hash_term(Hashable,Hash),IxClause,clause(Data,_,Ref)).
    
cmpix_assert(Names,Tmpl,Data,Ref,Assert):-
    index_name(Tmpl,Names,IxName),
    IxClause =..[IxName,Hash,Ref],
    cmpix_hashable(Names,Tmpl,Data,Hashable),
    Assert=(hash_term(Hashable,Hash),assert(IxClause)).
    

cmpix_retractall(Names,Tmpl,Data,Action,Ref,Retract):-
    index_name(Tmpl,Names,IxName),
    IxClause =..[IxName,Hash,Ref],
    cmpix_hashable(Names,Tmpl,Data,Hashable),
    Retract=
    	(	ground(Hashable) 
    	->  hash_term(Hashable,Hash),
    		forall(
    			(	IxClause,
    				clause(Data,_,Ref)
    			),
    			Action
    		)
    	).

cmpix_retract_action(Names,Tmpl,Data,Ref,Action):-
	index_name(Tmpl,Names,IxName),
    IxClause =..[IxName,Hash,Ref],
    cmpix_hashable(Names,Tmpl,Data,Hashable),
    Action=(hash_term(Hashable,Hash),retract(IxClause)).

    
map_names_to_args([],_,_,[]).
map_names_to_args([Name|Names],Tmpl,Data,[Arg|Args]):-
    arg(I,Tmpl,Name),
    arg(I,Data,Arg),
    map_names_to_args(Names,Tmpl,Data,Args).

   
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
    create_undoable_retract(Template,Cx,Decls,Retracts),
    Decl=(Head:-Getter,Retracts),    
    ExpDecl= (:- export(Head)),
    memberchk(Decl,Decls),
    memberchk(ExpDecl,Decls).



create_retract(Template,Cx,Decls,RetractOut):-
    functor(Cx,Name,Arity),
    create_retract_action(Template,Cx,Decls,Ref,Action),
    
    create_retract_args(Arity,Template,Cx,Decls,Name,Ref,Action,forall(clause(Cx,_,Ref),Action),Retract),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->	cmpix_retractall(Names,Template,Cx,Action,Ref,CmpixRetract),
    	RetractOut=(CmpixRetract;Retract)
    ;	RetractOut=Retract
    ).
create_undoable_retract(Template,Cx,Decls,RetractOut):-
    functor(Cx,Name,Arity),
    create_retract_action(Template,Cx,Decls,Ref,Action0),
    undo_retract_head(Cx,UndoGoal),
    Action=
    	(	Action0,
    		(	'$recording'(Key,full)
			->	recorda(Key,UndoGoal)
			;	true
			)
		),
    create_retract_args(Arity,Template,Cx,Decls,Name,Ref,Action,forall(clause(Cx,_,Ref),Action),Retract),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->	cmpix_retractall(Names,Template,Cx,Action,Ref,CmpixRetract),
    	RetractOut=(CmpixRetract;Retract)
    ;	RetractOut=Retract
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

create_retract_action(Template,Cx,Decls,Ref,ActionOut):-
    functor(Cx,Name,Arity),
    create_retract_action_args(Arity,Name,Template,Cx,Decls,Ref,erase(Ref),Action),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->  cmpix_retract_action(Names,Template,Cx,Ref,CmpxAction),
    	ActionOut=(Action,CmpxAction)
    ;	ActionOut=Action    
    ).

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



create_inverse_retract_action(Template,Cx,Decls,Ref,ActionOut):-
    functor(Cx,Name,Arity),
    create_inverse_retract_action_args(Arity,Name,Template,Cx,Decls,Ref,assert(Cx,Ref),Action),
    (	decls_query(Decls,'$metapef_type_tag'(Name,composite_index(Names)))
    ->  inverse_cmpix_retract_action(Names,Template,Cx,Ref,CmpxAction),
    	ActionOut=(Action,CmpxAction)
    ;	ActionOut=Action    
    ).

create_inverse_retract_action_args(0,_Name,_Template,_Cx,_Decls,_Ref,Retracts,Retracts):-!.
create_inverse_retract_action_args(I,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut):-
    decls_query(Decls,'$metapef_attribute_tag'(Name,I,index)),
    !,
    arg(I,Cx,Arg),
	index_name(Template,I,IxName),
    IndexClause=..[IxName,Arg,Ref],
	J is I - 1,
	create_inverse_retract_action_args(J,Name,Template,Cx,Decls,Ref, (assert(IndexClause),Retracts),RetractsOut).  
create_inverse_retract_action_args(I,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut):-
    J is I - 1,
	create_inverse_retract_action_args(J,Name,Template,Cx,Decls,Ref,Retracts,RetractsOut).  

inverse_cmpix_retract_action(Names,Tmpl,Data,Ref,Action):-
	index_name(Tmpl,Names,IxName),
    IxClause =..[IxName,Hash,Ref],
    cmpix_hashable(Names,Tmpl,Data,Hashable),
    Action=(hash_term(Hashable,Hash),index(IxClause)).


process_indices(Decls,_):-
    var(Decls),
    !.
process_indices(['$metapef_attribute_tag'(Name,Num,index)|Decls],Tmpl):-
    functor(Tmpl,Name,_),
    !,
    index_name(Tmpl,Num,IxName),
    memberchk((:- dynamic IxName/2),Decls),
    process_indices(Decls,Tmpl).
process_indices(['$metapef_type_tag'(Name,composite_index(Names))|Decls],Tmpl):-
    functor(Tmpl,Name,_),
    !,
    index_name(Tmpl,Names,IxName),
    memberchk((:- dynamic IxName/2),Decls),
    process_indices(Decls,Tmpl).

process_indices([_|Decls],Tmpl):-    
    process_indices(Decls,Tmpl).


undo_retract_head(Cx,Head):-
    Cx=..[Name|Args],
	concat_atom([undo,Name,retract],'_',UndoName),
	Head=..[UndoName|Args].

undo_assert_head(Cx,Head):-
    Cx=..[Name|Args],
	concat_atom([undo,Name,assert],'_',UndoName),
	Head=..[UndoName|Args].


undo_retract_clause(Template,Cx,Decls,(Head:-Asserts)):-
    undo_retract_head(Cx,Head),
    create_index_asserts(Template,Cx,Decls,Asserts,_).
    
undo_assert_clause(Template,Cx,Decls,(Head:-Retracts)):-
    undo_assert_head(Cx,Head),
    create_retract(Template,Cx,Decls,Retracts).


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

metapef_clause(Decl,'$metapef_is_a'(Type,SuperType)):-
    type_decl_name(Decl,Type),
    type_decl_supertypes(Decl,SuperTypes),
    member(SuperType,SuperTypes).
metapef_clause(Decl,'$metapef_edge'(Type,I,AttrType)):-
    type_decl_name(Decl,Type),
	type_decl_attributes(Decl,Attrs),
	arg(I,Attrs,AttrDecl),
	attr_decl_types(AttrDecl,AttrTypes),
	member(AttrType,AttrTypes).
metapef_clause(Decl,'$metapef_type_tag'(Type,Tag)):-
    type_decl_name(Decl,Type),
	type_decl_tags(Decl,Tags),
	member(Tag,Tags).
metapef_clause(Decl,'$metapef_attribute_tag'(Type,I,AttrTag)):-
    type_decl_name(Decl,Type),
	type_decl_attributes(Decl,Attrs),
	arg(I,Attrs,AttrDecl),
	attr_decl_tags(AttrDecl,AttrTags),
	member(AttrTag,AttrTags).



type_decl_attribute_names(Decl,Names):-
    type_decl_attributes(Decl,Attrs),
    findall(Name,
    	(	arg(_,Attrs,AttrDecl),
    		attr_decl_name(AttrDecl,Name)
    	),
    	Names
    ).

type_decl_untyped_template(Decl,Tmpl):-
    type_decl_name(Decl,Type),
	type_decl_attribute_names(Decl,AttrNames),
	Tmpl=..[Type|AttrNames].

index_directive(Decl,	(:- dynamic IxName/2) ):-
    type_decl_attributes(Decl,Attrs),
    type_decl_untyped_template(Decl,Tmpl),
    arg(I,Attrs,AttrDecl),
    attr_decl_tags(AttrDecl,Tags),
    memberchk(index,Tags),
    index_name(Tmpl,I,IxName).
index_directive(Decl,	(:- dynamic IxName/2) ):-
	type_decl_tags(Decl,Tags),
	type_decl_untyped_template(Decl,Tmpl),
	member(composite_index(Names),Tags),
	index_name(Tmpl,Names,IxName).
    
	
%--------------	
expand_definition(TypedTemplate0,Decls):-						  
    parse_type_declaration(TypedTemplate0,TypeDecl0),
    (	'$option'(implicit_ids)
    ->	ensure_has_id(TypeDecl0,TypeDecl1)
    ;	TypeDecl1=TypeDecl0
    ),
    
    (	'$option'(index_foreign_keys)
    ->	ensure_foreign_keys_are_indexed(TypeDecl1,TypeDecl)
    ;	TypeDecl=TypeDecl1
    ),
    /*strip_types(TypedTemplate,Template),
    functor(Template,Name,Arity),
    */
    type_decl_name(TypeDecl,Name),
    type_decl_attributes(TypeDecl,Attrs),
    type_decl_untyped_template(TypeDecl,Template),
    functor(Attrs,_,Arity),
    
    /*                    
    process_types(TypedTemplate,Decls),    
    process_indices(Decls,Template),*/
    findall(Clause,metapef_clause(TypeDecl,Clause),Clauses),
    findall(Directive,index_directive(TypeDecl,Directive),Directives),
    flatten([(:-dynamic(Name/Arity)),Directives,Clauses],Decls0),
    append(Decls0,_,Decls),
    
    
	define_assert(Template,Decls),
    define_retractall(Template,Decls),
    define_query(Template,Decls),
    define_query2(Template,Decls),
    %begin debug
    functor(Cx,Name,Arity),
    %create_inverse_retract_action(Template,Cx,Decls,Ref,ActionOut),
    undo_retract_clause(Template,Cx,Decls,URC),
    undo_assert_clause(Template,Cx,Decls,UAC),
    memberchk(URC,Decls),
    memberchk(UAC,Decls),
    %end debug
    expand_term((:-pdt_define_context(Template)),ContextDecls),
	    	
	findall(Decl,pdt_util_context:export_decl(Template,Decl),ContextExportDecls),	
    append(ContextDecls,ContextExportDecls,TailDecls),
    has_tail(['$metapef_concrete'(Name), '$metapef_template'(Name,Template)|TailDecls],Decls).


ensure_foreign_keys_are_indexed(In,Out):-
	type_decl_attributes(In,AttrsIn),
	AttrsIn=..[F|AttrDeclsIn],
	ensure_foreign_keys_are_indexed2(AttrDeclsIn,AttrDeclsOut),
	AttrsOut=..[F|AttrDeclsOut],
	type_decl_set_attributes(In,AttrsOut,Out).
	
ensure_foreign_keys_are_indexed2([],[]).
ensure_foreign_keys_are_indexed2([AttrDeclIn|AttrDeclsIn],[AttrDeclOut|AttrDeclsOut]):-
	attr_decl_tags(AttrDeclIn,Tags),
	attr_decl_types(AttrDeclIn,Types),
	(	memberchk(index,Tags)
	->	AttrDeclOut=AttrDeclIn
	;	Types ==[]
	->	AttrDeclOut=AttrDeclIn
	;	attr_decl_set_tags(AttrDeclIn,[index|Tags],AttrDeclOut)
	),
	ensure_foreign_keys_are_indexed2(AttrDeclsIn,AttrDeclsOut).
	
ensure_has_id(In,Out):-
    type_decl_attribute_names(In,Names),
    (	memberchk(id,Names)
    ->	Out=In
    ;	type_decl_attributes(In,AttrsIn),
	AttrsIn=..[F|AttrDeclsIn],
	attr_decl_new(IdDcl),
	attr_decl_num(IdDcl,Num),
        attr_decl_name(IdDcl,id),
        attr_decl_types(IdDcl,[]),
        attr_decl_tags(IdDcl,[index]),
	append(AttrDeclsIn,[IdDcl],AttrDeclsOut),
	AttrsOut=..[F|AttrDeclsOut],
	functor(AttrsOut,_,Num),
	type_decl_set_attributes(In,AttrsOut,Out)
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
    
