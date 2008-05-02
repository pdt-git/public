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
	type_decl_assert(TypeDecl,Data,Assert),
	type_decl_cmpix_asserts(TypeDecl,Data,CmpIxHashes,CmpIxAsserts),
	type_decl_revix_asserts(TypeDecl,Data,RevIxAsserts),
	type_decl_tags(TypeDecl,Tags),
	(	memberchk(no_cleanup,Tags)
	->	AssertAndRecord=(CmpIxHashes,Assert,CmpIxAsserts,RevIxAsserts)
	;	type_decl_record_assert(TypeDecl,Data,Record),
		AssertAndRecord=(CmpIxHashes,Assert,CmpIxAsserts,RevIxAsserts,Record)
	).    
    	
type_decl_assert(TypeDecl,Data,assert(Data)):-
    type_decl_name(TypeDecl,Name),
	type_decl_arity(TypeDecl,Arity),
	functor(Data,Name,Arity).
	

type_decl_id_check(TypeDecl,Data,IdCheck):-
    (	type_decl_attr_by_name(TypeDecl,id,AttrDecl)
    ->	attr_decl_num(AttrDecl,IdNum),
    	arg(IdNum,Data,Id),
    	type_decl_name(TypeDecl,Type),
    	IdCheck=(var(Id)->pef_reserve_id(Type,Id);true)
    ;	IdCheck=true
    ).
    
    


type_decl_record_assert(TypeDecl,Data,Record):-
    type_decl_undo_assert_head(TypeDecl,Data,UndoGoal),
    Record=
    	(	'$recording'(Key,_)
    	->	asserta('$undo'(Key,UndoGoal))
    	;	true
    	).

type_decl_record_retract(TypeDecl,Data,Record):-
    type_decl_undo_retract_head(TypeDecl,Data,UndoGoal),
    Record=
    	(	'$recording'(Key,full)
    	->	asserta('$undo'(Key,UndoGoal))
    	;	true
    	).

    	

    

type_decl_revix_asserts(TypeDecl,Data,RevIxAsserts):-
    (	bagof(RevIxAssert,type_decl_revix_assert(TypeDecl,Data,RevIxAssert),RevIxAsserts0)
    ->	list_conjunction(RevIxAsserts0,RevIxAsserts)
    ;	RevIxAsserts=true
    ).
type_decl_cmpix_asserts(TypeDecl,Data,CmpIxHashes,CmpIxAsserts):-
    (	bagof(
    		pair(CmpIxHash,CmpIxAssert),
    		type_decl_cmpix_assert(TypeDecl,Data,CmpIxHash,CmpIxAssert),
    		Pairs
    	)    	
    ->	split_pairs(Pairs,CmpIxHashes0,CmpIxAsserts0),
    	list_conjunction(CmpIxHashes0,CmpIxHashes),    	
    	list_conjunction(CmpIxAsserts0,CmpIxAsserts)
    ;	CmpIxAsserts=true,
    	CmpIxHashes=true
    ).


split_pairs([],[],[]).
split_pairs([pair(A,B)|Pairs],[A|As],[B|Bs]):-
    split_pairs(Pairs,As,Bs).
    
list_conjunction([],true).
list_conjunction([A|List],(A,Conjunction)):-
    list_conjunction(List,Conjunction).

list_disjunction_or_true([],true).
list_disjunction_or_true([A|As], (A;Disjunction)):-
	list_disjunction_or_true(As,Disjunction).


type_decl_revix_assert(TypeDecl,Data,RevIxAssert):-
    type_decl_revix_name(TypeDecl,RevIxAttrDecl,RevIxName),
    type_decl_attr_by_name(TypeDecl,id,IdAttrDecl),
    attr_decl_num(RevIxAttrDecl,RevIxNum),
    attr_decl_num(IdAttrDecl,IdNum),
    arg(IdNum,Data,Id),
    arg(RevIxNum,Data,Key),
    RevIxClause =.. [RevIxName,Key,Id],
    RevIxAssert = assert(RevIxClause).
 
 type_decl_cmpix_assert(TypeDecl,Data,CmpIxHash,CmpIxAssert):-
 	type_decl_cmpix_name(TypeDecl,AttrNames,CmpIxName),
 	type_decl_cmpix_hashable(TypeDecl,AttrNames,Data,Hashable),
 	type_decl_attr_by_name(TypeDecl,id,IdAttrDecl),
    attr_decl_num(IdAttrDecl,IdNum),
    arg(IdNum,Data,Id),
    CmpIxHash = hash_term(Hashable,Key),
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
    type_decl_cmpix_hashable_X(AttrNames,TypeDecl,Data,AttrValues),
    Hashable=..[h|AttrValues].

type_decl_cmpix_hashable_X([],_,_,[]).
type_decl_cmpix_hashable_X([AttrName|AttrNames],TypeDecl,Data,[AttrValue|AttrValues]):-
    once(type_decl_attr_by_name(TypeDecl,AttrName,AttrDecl)),
    attr_decl_num(AttrDecl,Num),
    arg(Num,Data,AttrValue),
    type_decl_cmpix_hashable_X(AttrNames,TypeDecl,Data,AttrValues).

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
    
 
    


type_decl_query_clause(TypeDecl,
	(	Head:-
			Getter,			
			IxQueries,
			Data
	)
):-
    type_decl_name(TypeDecl,TypeName),
    type_decl_getter(TypeDecl,AttrList,Data,Getter),
    type_decl_index_queries(TypeDecl,Data,IxQueries),
    atom_concat(TypeName,'_query',HeadName),
    (	Head=..[HeadName,AttrList]
    ;	Head=..[HeadName,AttrList,Data]
    ).

type_decl_index_queries(TypeDecl,Data,IxQueries):-
    (	bagof(CmpIxQuery,type_decl_cmpix_query(TypeDecl,Data,CmpIxQuery),CmpIxQueries)
	->	true
	;	CmpIxQueries=[]
	),
	(	bagof(RevIxQuery,type_decl_revix_query(TypeDecl,Data,RevIxQuery),RevIxQueries)
	->	true
	;	RevIxQueries=[]
	),
	append(CmpIxQueries,RevIxQueries,IxQueries0),
	list_disjunction_or_true(IxQueries0,IxQueries).



type_decl_cmpix_query(TypeDecl,Data,
	(	ground(Hashable) 
	-> 	hash_term(Hashable,Key),
		CmpIxGoal
	)
):-    
    type_decl_cmpix_name(TypeDecl,AttrNames,CmpIxName),
    type_decl_cmpix_hashable(TypeDecl,AttrNames,Data,Hashable),
    type_decl_attr_by_name(TypeDecl,id,IdDecl),
    attr_decl_num(IdDecl,IdNum),
    arg(IdNum,Data,Id),
	CmpIxGoal =.. [CmpIxName,Key,Id].    
    

type_decl_revix_query(TypeDecl,Data,(nonvar(Key) -> RevIxGoal)):-    
    type_decl_revix_name(TypeDecl,KeyDecl,RevIxName),    
    type_decl_attr_by_name(TypeDecl,id,IdDecl),
    attr_decl_num(IdDecl,IdNum),
    attr_decl_num(KeyDecl,KeyNum),
    arg(IdNum,Data,Id),
    arg(KeyNum,Data,Key),
	RevIxGoal =.. [RevIxName,Key,Id].    






type_decl_retract_clause(TypeDecl,
	(	Head:-
			Getter,			
			IxQueries,
			retract(Data),
			IxRetracts,
			RecordUndo
	)
):-
    type_decl_name(TypeDecl,TypeName),
    type_decl_getter(TypeDecl,AttrList,Data,Getter),
    type_decl_index_queries(TypeDecl,Data,IxQueries),
    type_decl_index_retracts(TypeDecl,Data,IxRetracts),
    type_decl_record_retract(TypeDecl,Data,RecordUndo),
    atom_concat(TypeName,'_retract',HeadName),
    (	Head=..[HeadName,AttrList]
    ;	Head=..[HeadName,AttrList,Data]
    ).

type_decl_retractall_clause(TypeDecl,
	(	Head:-
			Getter,			
			forall(	
				(	IxQueries,
					retract(Data)
				),
				(	IxRetracts,
					RecordUndo
				)
			)
	)
):-
    type_decl_name(TypeDecl,TypeName),
    type_decl_getter(TypeDecl,AttrList,Data,Getter),
    type_decl_index_queries(TypeDecl,Data,IxQueries),
    type_decl_index_retracts(TypeDecl,Data,IxRetracts),
    type_decl_record_retract(TypeDecl,Data,RecordUndo),
    atom_concat(TypeName,'_retractall',HeadName),
    (	Head=..[HeadName,AttrList]
    ;	Head=..[HeadName,AttrList,Data]
    ).
    		

type_decl_index_retracts(TypeDecl,Data,IxRetracts):-    
    (	bagof(pair(CmpIxHash,CmpIxRetract),type_decl_cmpix_retract(TypeDecl,Data,CmpIxHash,CmpIxRetract),Pairs)
    ->	split_pairs(Pairs,CmpIxHashes,CmpIxRetracts)
    ;	CmpIxRetracts=[],
    	CmpIxHashes=[]    
    ),
    (	bagof(RevIxRetract,type_decl_revix_retract(TypeDecl,Data,RevIxRetract),RevIxRetracts)
    ->	true
    ;	RevIxRetracts=[]
    ),
	append(CmpIxRetracts,RevIxRetracts,IxRetracts0),
	append(CmpIxHashes,IxRetracts0,IxRetracts1),
	list_conjunction(IxRetracts1,IxRetracts).
	

type_decl_cmpix_retract(TypeDecl,Data,hash_term(Hashable,Key),retract(CmpIxGoal)):-    
    type_decl_cmpix_name(TypeDecl,AttrNames,CmpIxName),
    type_decl_cmpix_hashable(TypeDecl,AttrNames,Data,Hashable),
    type_decl_attr_by_name(TypeDecl,id,IdDecl),
    attr_decl_num(IdDecl,IdNum),
    arg(IdNum,Data,Id),
	CmpIxGoal =.. [CmpIxName,Key,Id]. 
	
type_decl_revix_retract(TypeDecl,Data,retract(RevIxGoal)):-    
    type_decl_revix_name(TypeDecl,KeyDecl,RevIxName),    
    type_decl_attr_by_name(TypeDecl,id,IdDecl),
    attr_decl_num(IdDecl,IdNum),
    attr_decl_num(KeyDecl,KeyNum),
    arg(IdNum,Data,Id),
    arg(KeyNum,Data,Key),
	RevIxGoal =.. [RevIxName,Key,Id].  	

type_decl_arity(Decl,Arity):-
    type_decl_attributes(Decl,Attrs),
    functor(Attrs,_,Arity).
    
type_decl_getter(TypeDecl,AttrList,Data,Getter):-
    type_decl_name(TypeDecl,Name),
	type_decl_arity(TypeDecl,Arity),
	functor(Data,Name,Arity),
	atom_concat(Name,'_get',GetterName),
	Getter=..[GetterName,Data,AttrList].

type_decl_undo_assert_head(Decl,Data,Head):-
	type_decl_name(Decl,Name),
	type_decl_arity(Decl,Arity),
	functor(Data,Name,Arity),
	atom_concat(Name,'_undo_assert',HeadName),
	Data=..[Name|Args],
	Head=..[HeadName|Args].

type_decl_undo_retract_head(Decl,Data,Head):-
	type_decl_name(Decl,Name),
	type_decl_arity(Decl,Arity),
	functor(Data,Name,Arity),
	atom_concat(Name,'_undo_retract',HeadName),
	Data=..[Name|Args],
	Head=..[HeadName|Args].


type_decl_undo_clause(Decl,
	(	Head:-
    		IndexQueries,
    		retract(Data),
    		IndexRetracts
	)
):-
    type_decl_name(Decl,Name),
	type_decl_arity(Decl,Arity),
	type_decl_undo_assert_head(Decl,Data,Head),
	functor(Data,Name,Arity),    
	type_decl_index_queries(Decl,Data,IndexQueries),
	type_decl_index_retracts(Decl,Data,IndexRetracts).

type_decl_undo_clause(Decl,
	(	Head:-
    		CmpIxHashes,
    		assert(Data),    		
    		CmpIxAsserts,
    		RevIxAsserts
	)
):-
    type_decl_name(Decl,Name),
	type_decl_arity(Decl,Arity),
	functor(Data,Name,Arity),    
	type_decl_undo_retract_head(Decl,Data,Head),	
	type_decl_cmpix_asserts(Decl,Data,CmpIxHashes,CmpIxAsserts),
	type_decl_revix_asserts(Decl,Data,RevIxAsserts).	


type_decl_metapef_clause(Decl,'$metapef_concrete'(Name)):-
    type_decl_name(Decl,Name).
type_decl_metapef_clause(Decl, '$metapef_template'(Name,Template)):-
    type_decl_name(Decl,Name),
    type_decl_untyped_template(Decl,Template).
type_decl_metapef_clause(Decl, '$metapef_type_decl'(Name,Decl)):-
    type_decl_name(Decl,Name).
type_decl_metapef_clause(Decl,'$metapef_is_a'(Type,SuperType)):-
    type_decl_name(Decl,Type),
    type_decl_supertypes(Decl,SuperTypes),
    member(SuperType,SuperTypes).
type_decl_metapef_clause(Decl,'$metapef_edge'(Type,I,AttrType)):-
    type_decl_name(Decl,Type),
	type_decl_attributes(Decl,Attrs),
	arg(I,Attrs,AttrDecl),
	attr_decl_types(AttrDecl,AttrTypes),
	member(AttrType,AttrTypes).
type_decl_metapef_clause(Decl,'$metapef_type_tag'(Type,Tag)):-
    type_decl_name(Decl,Type),
	type_decl_tags(Decl,Tags),
	member(Tag,Tags).
type_decl_metapef_clause(Decl,'$metapef_attribute_tag'(Type,I,AttrTag)):-
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

type_decl_index_directive(Decl,	(:- dynamic IxName/2) ):-    
    type_decl_revix_name(Decl,_,IxName).
type_decl_index_directive(Decl,	(:- dynamic IxName/2) ):-    
    type_decl_cmpix_name(Decl,_,IxName).
    
	


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
		attr_decl_num(IdDcl,1),
	    attr_decl_name(IdDcl,id),
	    attr_decl_types(IdDcl,[]),
	    attr_decl_tags(IdDcl,[]),
	    shift_attr_num(AttrDeclsIn,AttrDeclsOut),				
		AttrsOut=..[F,IdDcl|AttrDeclsOut],		
		type_decl_set_attributes(In,AttrsOut,Out)
    ).

shift_attr_num([],[]).
shift_attr_num([AttrDeclIn|AttrDeclsIn],[AttrDeclOut|AttrDeclsOut]):-
    attr_decl_num(AttrDeclIn,Num),
    Num2 is Num + 1,
    attr_decl_set_num(AttrDeclIn,Num2,AttrDeclOut),
    shift_attr_num(AttrDeclsIn,AttrDeclsOut).



    
    
   /* 
assert_sequence(TypeDecl,
	ct(Head,
		condition(
			Getter, %bind head variables
			(	var(Id) %make sure the ID is set.
			->	pef_reserve_id(Type,Id)
			;	true
			),
			CmpIxCondition
		),
		transformation(
			DataAssert,
			IxAssert,
			CmpIxAssert,
			UndoAssert
		)
	)
):-
   type_decl_name(TypeDecl,Type),
   atom_concat(Type,'_assert',HeadName),
   Head=..[HeadName,AttrList],
   type_decl_getter(TypeDecl,Getter,AttrList,Data),
   type_decl_id(TypeDecl,Data,Id),
   type_decl_cmpix_condition(TypeDecl,Data,CmpIxCondition),
   type_decl_data_assert(TypeDecl,Data,DataAssert),
   
  */
  
  
expand_type_decl(TypeDecl,Out):-
    (	type_decl_metapef_clause(TypeDecl,Out)
    ;	type_decl_index_directive(TypeDecl,Out)
    ;	type_decl_assert_clause(TypeDecl,Out)
    ;	type_decl_retract_clause(TypeDecl,Out)
    ;	type_decl_retractall_clause(TypeDecl,Out)
    ;	type_decl_query_clause(TypeDecl,Out)
    ;	type_decl_undo_clause(TypeDecl,Out)
    ;	type_decl_untyped_template(TypeDecl,Template),
    	expand_term((:-pdt_define_context(Template)),ContextDecls),
    	member(Out,ContextDecls)	    	
	;	type_decl_untyped_template(TypeDecl,Template),
		pdt_util_context:export_decl(Template,Out)
    ).
  
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
    findall(Decl,expand_type_decl(TypeDecl,Decl),Decls).        
    	
    
    
  
 user:term_expansion((:- define_pef(TypedTemplate0)),Decls):-    	
	pef_base:expand_definition(TypedTemplate0,Decls).
  