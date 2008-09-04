:- module(pef_base,
	[	pef_reserve_id/2, 
		pef_type/2,
		pef_start_recording/1,
		pef_start_recording/2,
		pef_stop_recording/0,
		pef_clear_record/1,
		pef_count/2,
		pef_record_key/2,
		pef_type_tag/2,
		pef_type_is_a/2,
%		pef_last_record/2,
		pef_node/3,
		pef_edge/3,
		pef_delete/1,
		metapef_template/2,
		metapef_ref/3,
		pef_generic_query/3
	]
).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	

% these rules are generated during the expansion of the pef definitions
:- discontiguous '$metapef_template'/2, 
				 '$metapef_concrete'/1,
				 '$metapef_edge'/3,
				 '$metapef_attribute_tag'/3,
				 '$metapef_type_tag'/2,
				 '$metapef_is_a'/2,
				 '$metapef_type_decl'/2 .


:- dynamic '$option'/1.
:- multifile '$option'/1.

:- dynamic pef_before_assert_hook/2, pef_before_retract_hook/2, pef_after_assert_hook/2, pef_after_retract_hook/2.
:- multifile pef_before_assert_hook/2, pef_before_retract_hook/2, pef_after_assert_hook/2, pef_after_retract_hook/2.


:- thread_local '$recording'/2.
:- dynamic '$recording'/2.
:- dynamic '$record_key'/2.

:- dynamic pef_type/2.

% use @ to attach "tags" to attributes
% you can "chain" several tags to one attribute.
:- op(600,xfy,@).

/* 2007-10-25 NOTE TO MYSELF: Before thinking about performance improvements, read this first!!
'$erased'/1, my_erase/2 and stuff:


trying to speed up the auto-clean (see pef_clear_record/1).

The  assert predicates will record the clauses the are adding to records typically associated
to build targets. When the targets are rebuild, all those clauses are erased first.

This introduces one problem, though:
When clauses are removed using one of the retract predicates, the record contains dangling references.
SWI-Prolog does not offer a save way to test wether a reference is still valid or not.

Until now I solved the problem by not erasing the clauses directly, but by calling the retractall predicates
instead. This is save, but slow: the cleanup phase, in particular the lookup involved in the retract/retractall calls
becomes a dominant factor in the time requirements of builds. (up to 60% in recent profilings)

I suspect the reason for this to be that the first argument of the index clause is often not atomic, which 
seems to make hashing more expensive. My theory is supported by the fact that the 
pef_toplevel reverse indices are by far the most expensive ones in this respect. In fact, the term argument is
indexed. Its values are probably the most complex argument terms currently represented in the pef base.

I will try two things:
1) remove the index. I don't think it is really used. 
2) if a need should arise to index complex arguments, I can try the following:

- assert predicates record references to created predicates.
- retractall predicates keep track of the clauses they retract. (the '$erase'/1 predicate)
- during auto-cleanup, run through the recorded references, using the '$erase'/1 predicate to filter out
  those that are already deleted. The  '$erase'/1 lookups should be much cheaper than the "real" index lookups,
  since we are only hashing atomic values here.

:- dynamic '$erased'/1.
my_erase(Ref,Refs):-
    (	clause('$erased'(Ref),_,ERef)
    ->	erase(ERef)
    ;	erase_elms([Ref|Refs])
    ).
erase_elms([]).
erase_elms([Ref|Refs]):-
    erase(Ref),
    erase_elms(Refs).

*/



pef_reserve_id(Type,Id):-
    flag(pef_next_id,Id,Id + 1),
    assert(pef_type(Id,Type)). 

    
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



%% pef_type_tag(?Type,?Tag).
% succeeds if type Type is tagged with Tag.
pef_type_tag(Type,Tag):-
    '$metapef_type_tag'(Type,Tag).
%% pef_type_is_a(?Type1, ?Type2).
% succeeds if Type1 is a subtype of Type2.
pef_type_is_a(Type1,Type2):-
    '$metapef_is_a'(Type1,Type2).


pef_start_recording(Term):-
	record_key(Term,Key),
	asserta('$recording'(Key,asserts)).
pef_start_recording(Term,Mode):-
	record_key(Term,Key),
	asserta('$recording'(Key,Mode)).

/*
pef_last_record(Term,Ref):-
    record_key(Term,Key),
    recorded(Key,Ref),
    !.
pef_last_record(_,[]).
*/
	
pef_stop_recording:-
	retract('$recording'(_,_)),
	!.
pef_stop_recording:-
    throw(not_recording).

/*
pef_clear_record(Term):-
    record_key(Term,Key),
    forall(
    	recorded(Key,CleanupGoal,RecordRef),
    	(	call(CleanupGoal),
    		erase(RecordRef)
    	)
    ).
*/
pef_clear_record(Term):-
    record_key(Term,Key),
    forall(
    	retract('$undo'(Key,CleanupGoal)),
    	call(CleanupGoal)
    ).


pef_record_key(Term,Key):-
    record_key(Term,Key).
    
record_key(Term,Key):-
    '$record_key'(Term,Key),
    !.
record_key(Term,Key):-
	pef_reserve_id('$record_key',Key),
	assert('$record_key'(Term,Key)).

find_id(Tmpl,Num):-
    arg(Num,Tmpl,id),
    !.


%% pef_delete(+Node).
%  Delete a pef and all pefs (directly or indirectly) referencing it.
%  This is a generic and slow implementation. Should not be used in "burst" situations.  
%  A specialized version would be nice, i.e. one that avoids runtime construction of functors. 
%  Maybe this could be achieved through partial evaluation?
pef_delete(Node):-
    forall(
    	pef_edge(Referer,_,Node),
    	pef_delete(Referer)
    ),
    delete_X(Node).    

delete_X(Id):-
    pef_type(Id,Type),
    atom_concat(Type,'_retractall',Functor),
    Goal =.. [Functor,[id=Id]],
    call(Goal).


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

metapef_index_arg(Type,Arg):-
    '$metapef_attribute_tag'(Type,Arg,index).


    
    
    
metapef_ref(Type,RefType,RefArg):-
    metapef_is_a(Type,TargetType),
    '$metapef_edge'(RefType,RefArg,TargetType).



pef_edge(From,ArgName,To):-
    (	nonvar(To)
    ->	pef_type(To,ToT)
    ;	true
    ),
    (	nonvar(From)
    ->	pef_type(From,FromT)
    ;	true
    ),
    pef_edge(From,FromT,ArgName,To,ToT).
    
pef_edge(From,FromT,ArgName,To,ToT):-
    (	var(From),nonvar(To)
    ->	'$pef_inverse_edge'(To,ToT,ArgName,From,FromT)
    ;	'$pef_edge'(From,FromT,ArgName,To,ToT)
    ).

pef_node(Id,Type,Labels):-
    '$pef_node'(Type,Id,Labels).

valid_target(ToT,To):-
    pef_type(To,ToT).


metapef_template(Type,Template):-
    '$metapef_template'(Type,Template).
    
pef_generic_query(Type,ArgList,Data):-
    atom_concat(Type,'_query',F),
    Query=..[F,ArgList,Data],
    call(Query).
	    


    

    
% rules for expanding and postprocessing pef definitions.	 
:- ensure_loaded(pef_base__term_expansion-v3).
:- ensure_loaded(pef_base__post_processing-v3).    

% PEF definitions moved to a separate file.
:- include(pef_definitions).
    
% IMPORTANT: the following line should stay at the end of the file. 
% They trigger the post-processing of the meta pefs that require a global perspective.
:- postprocess_pefs.
