/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( pdt_xref,
         [ find_reference_to/8 % +Term,+ExactMatch,
                               % -RefModule,-RefName,-RefArity,-RefFile,-RefLine,-PropertyList
         ]
         ).

:- use_module(pdt_prolog_library(utils4modules)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).

:- use_module( pdt_common_pl(properties), [properties_for_predicate/4] ).
:- use_module(library(lists)).

:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).

%:- ensure_loaded('../pdt_factbase.pl').
%:- use_module('../modules_and_visibility').
    /*********************************************
     * FIND REFERENCES TO A PARTICULAR PREDICATE *
     ********************************************/
find_unique( Goal ) :-
    setof( Goal, Goal, Set),
    member(Goal, Set).
    
:- dynamic(result/5).

assert_result(IsAlias, QGoal, Caller, Location) :-
	assert_result(IsAlias, QGoal, Caller, Location, _).
assert_result(IsAlias, QGoal, _, clause_term_position(Ref, TermPosition), Kind) :-
    QGoal = _:_Goal,
    assertz(result(IsAlias, QGoal, Ref, TermPosition, Kind)),
    !.
assert_result(IsAlias, QGoal, _, file_term_position(File, TermPosition), Kind) :-
    QGoal = _:_Goal,
    assertz(result(IsAlias, QGoal, File, TermPosition, Kind)),
    !.

assert_result(IsAlias, QGoal, _, clause(Ref), Kind) :-
    QGoal = _:_Goal,
    assertz(result(IsAlias, QGoal, Ref, no_term_position, Kind)),
    !.

assert_result(_,_,_,_,_).

assert_result(IsAlias, QGoal-clause_term_position(Ref, TermPosition)) :-
	QGoal = _:_Goal,
	assertz(result(IsAlias, QGoal, Ref, TermPosition, [])),
	!.

assert_result(_,_). 

%% find_reference_to(Term, ExactMatch, RefModule, RefName, RefArity, RefFile, Position, PropertyList)
find_reference_to(Term, ExactMatch, RefModule, RefName, RefArity, RefFile, Position, PropertyList) :-
	Term = predicate(SearchMod, _, Functor, Separator, Arity0),
	(	Separator == (//),
		nonvar(Arity0)
	->	Arity is Arity0 + 2
	;	Arity = Arity0
	),
	!,
	retractall(result(_, _, _, _, _)),
	(	var(Functor), var(SearchMod) -> !, fail ; true),
	perform_search(Functor, Arity, SearchMod, ExactMatch),
	!,
	retract(result(Alias, M0:ReferencingGoal, ClauseRefOrFile, Termposition, _)),
	(	atom(ClauseRefOrFile)
	->	RefFile = ClauseRefOrFile,
		Line = 1,
		once(module_of_file(RefFile, RefModule)),
		RefName = (initialization),
		RefArity = 1
	;	ClauseRef = ClauseRefOrFile,
		clause_property(ClauseRef, predicate(RefModule:RefName/RefArity)),
		(	nonvar(SearchMod),
			var(Functor),
			var(Arity)
		->	SearchMod \== RefModule
		;	true
		),
		clause_property(ClauseRef, file(RefFile)),
		% check that RefFile is not derived from another file
		clause_property(ClauseRef, source(RefFile)),
		clause_property(ClauseRef, line_count(Line))
	),
	properties_for_predicate(RefModule,RefName,RefArity,PropertyList0),
	(	(	Termposition = term_position(Start, End, _, _, _)
		;	Termposition = Start-End
		)
	->	format(atom(Position), '~w-~w', [Start, End])
	;	Position = Line
	),
%	format(atom(ReferencingGoalAsAtom), '~w', [ReferencingGoal]),
	[line(Line)|PropertyList0] = PropertyList1,
	(	functor(ReferencingGoal, N, A),
		declared_in_module(M0, N, A, M)
	->	(	Separator == (//)
		->	format(atom(Called), '~w//~w', [N, Arity0])
		;	format(atom(Called), '~w/~w', [N, A])
		),
		CalledProperty = label(Called),
		(	M \== RefModule
		->	format(atom(Prefix), '~w:', [M]),
			PrefixProperty = prefix(Prefix)
		;	true
		)
	;	true
	),
	(	nonvar(Alias)
	->	format(atom(AliasAtom), ' [alias for ~w]', [Alias]),
		PropertyList2 = [suffix(AliasAtom)|PropertyList1]
	;	PropertyList2 = PropertyList1
	),
	(	nonvar(PrefixProperty)
	->	PropertyList3 = [PrefixProperty|PropertyList2]
	;	PropertyList3 = PropertyList2
	),
	(	nonvar(CalledProperty)
	->	PropertyList = [CalledProperty|PropertyList3]
	;	PropertyList = PropertyList3
	).

perform_search(Functor, Arity, Module, ExactMatch) :-
	(	nonvar(Functor)
	->	setof(
			p(SearchModule, SearchFunctor, SearchArity, IsAlias),
			Module^Functor^Arity^ExactMatch^search_predicate_indicator(Module, Functor, Arity, ExactMatch, SearchModule, SearchFunctor, SearchArity, IsAlias),
			Predicates
			),
		member(p(SearchModule, SearchFunctor, SearchArity, IsAlias), Predicates)
	;	Module = SearchModule
	),
	(	nonvar(SearchFunctor),
		nonvar(SearchArity)
	->	functor(Goal, SearchFunctor, SearchArity)
	;	true
	),
	collect_candidates(SearchModule, SearchFunctor, SearchArity, Candidates),
	pdt_walk_code([trace_reference(SearchModule:Goal), predicates(Candidates), on_trace(pdt_xref:assert_result(IsAlias))]),
	fail.

perform_search(_Functor, _Arity, _SearchMod, _ExactMatch).

search_predicate_indicator(SearchModule0, SearchFunctor0, SearchArity, true, SearchModule, SearchFunctor, SearchArity, IsAlias) :-
	nonvar(SearchArity),
	!,
	(	declared_in_module(SearchModule0, SearchFunctor0, SearchArity, SearchModule0)
	*->	(	SearchModule0 = SearchModule,
			SearchFunctor0 = SearchFunctor
		;	possible_alias(SearchModule0, SearchFunctor0, SearchArity, SearchModule, SearchFunctor),
			IsAlias = SearchModule0:SearchFunctor0/SearchArity
		)
	;	SearchModule0 = SearchModule,
		SearchFunctor0 = SearchFunctor
	).

search_predicate_indicator(SearchModule0, SearchFunctor0, Arity, true, SearchModule, SearchFunctor, SearchArity, IsAlias) :-
	var(Arity),
	!,
	setof(
		M-A,
		declared_in_module(M, SearchFunctor0, A, M),
		MAs
	),
	member(SearchModule0-SearchArity, MAs),
	(	SearchModule0 = SearchModule,
		SearchFunctor0 = SearchFunctor
	;	possible_alias(SearchModule0, SearchFunctor0, SearchArity, SearchModule, SearchFunctor),
		IsAlias = SearchModule0:SearchFunctor0/SearchArity
	).

search_predicate_indicator(SearchModule0, Functor, SearchArity, false, SearchModule, SearchFunctor, SearchArity, IsAlias) :-
	nonvar(SearchArity),
	!,
	setof(
		M-F,
		(declared_in_module(M, F, SearchArity, M), once(sub_atom(F, _, _, _, Functor))),
		MFs
	),
	member(SearchModule0-SearchFunctor0, MFs),
	(	SearchModule0 = SearchModule,
		SearchFunctor0 = SearchFunctor
	;	possible_alias(SearchModule0, SearchFunctor0, SearchArity, SearchModule, SearchFunctor),
		IsAlias = SearchModule0:SearchFunctor0/SearchArity
	).

search_predicate_indicator(SearchModule0, Functor, Arity, false, SearchModule, SearchFunctor, SearchArity, IsAlias) :-
	var(Arity),
	!,
	setof(
		M-F-A,
		(declared_in_module(M, F, A, M), once(sub_atom(F, _, _, _, Functor))),
		MFAs
	),
	member(SearchModule0-SearchFunctor0-SearchArity, MFAs),
	(	SearchModule0 = SearchModule,
		SearchFunctor0 = SearchFunctor
	;	possible_alias(SearchModule0, SearchFunctor0, SearchArity, SearchModule, SearchFunctor),
		IsAlias = SearchModule0:SearchFunctor0/SearchArity
	).

possible_alias(Module, Name, Arity, ImportingModule, AliasName) :-
	functor(Head, Name, Arity),
	\+ predicate_property(Module:Head, multifile),
	predicate_property(Module:Head, file(File)),
	source_file_property(File, load_context(ImportingModule, _Position, Options)),
	memberchk(imports(Imports), Options),
	memberchk(Name/Arity as AliasName, Imports).

collect_candidates(SearchModule, SearchFunctor, SearchArity, Candidates) :-
	ensure_call_graph_generated,
	setof(Module:Name/Arity, (
		SearchModule^SearchFunctor^SearchArity^NumberOfCalls^calls(SearchModule, SearchFunctor, SearchArity, Module, Name, Arity, NumberOfCalls)
	), Candidates).
	
%find_reference_to(Functor,Arity,DefFile, SearchMod, ExactMatch,
%                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) :-
%    find_unique(  find_reference_to__(Functor,Arity,DefFile, SearchMod, ExactMatch,
%                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) ).
%    
%find_reference_to__(Functor,Arity,DefFile, SearchMod, ExactMatch,
%                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) :-                  
%	( nonvar(DefFile)
%    -> module_of_file(DefFile,SearchMod)
%    ; true % Defining File and defining Module remain free ==> 
%           % Search for references to independent definitions
%           % <-- Does that make sense???
%    ),
%%    ( var(Arity) % Need to backtrack over all declared Functor/Arity combinations:
%%    -> ( setof( Functor/Arity, SearchMod^current_predicate(SearchMod:Functor/Arity), Set),
%%         member(Functor/Arity, Set)
%%       )
%%    ; true % Arity already bound in input
%%    ),
%%    functor(SearchTerm,Functor,Arity),
%    pdt_xref_data(SearchMod:Functor/Arity,ExactMatch,RefModule:RefHead,Ref,Kind),
%
%    functor(RefHead,RefName,RefArity),
%    predicate_property(RefModule:RefHead,_),
%    nth_clause(RefModule:RefHead,Nth,Ref),
%    clause_property(Ref, file(RefFile)),
%    clause_property(Ref, line_count(RefLine)),
%    properties_for_predicate(RefModule,RefName,RefArity,PropertyList),
%    ( var(Functor) -> Functor = '' ; true),
%    ( var(Arity) -> Arity = '' ; true),
%    ( var(DefFile) -> DefFile = '' ; true),
%    ( var(SearchMod) -> SearchMod = '' ; true).
%
%go :- % To list all results quickly call 
%      % ?- pdt_xref:go, fail.
%    find_reference_to(defined_in_file,6,__DefFile, __DefModule,RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,_PropertyList),
%    format( '~a reference from ~a:~w clause ~a, line ~a, file ~n~a~n~n',
%            [Kind, RefModule,RefName,RefArity, Nth, RefLine, RefFile]
%    ).
%	
%
%pdt_xref_data(DefModule:DefFunctor/DefArity,ExactMatch,RefModule:RefHead,Ref, Kind) :-
%	current_predicate(RefModule:F/A),     % For all defined predicates
%	functor(RefHead,F,A),   
%	nth_clause(RefModule:RefHead,_N,Ref),   % For all their clauses
%	'$xr_member'(Ref, QualifiedTerm),					% Get a term referenced by that clause
%	(	var(DefFunctor),
%		QualifiedTerm = M:_
%	->	M \== RefModule
%	;	true
%	), 
%	is_reference_to(DefModule:DefFunctor/DefArity,ExactMatch,RefHead,QualifiedTerm,Kind).     % (via SWI-Prolog's internal xref data)
% 
%
%   
%%pdt_xref_data(DefModule:DefHead,RefModule:RefHead,Ref, Kind) :-
%%    functor(DefHead, DefFunctor, DefArity),
%%    modules_and_visibility:get_predicate_referenced_as(DefModule, DefFunctor, DefArity, DefId),
%%    (	DefId = predefined(DeclModule, Functor, Arity)
%%    ->	parse_util:call_built_in(Functor, Arity, DeclModule, RefLitId)
%%    ;	parse_util:call_edge(DefId, RefLitId)
%%    ),
%%    parse_util:literalT(RefLitId, _, RefClauseId, _, _, _),
%%    parse_util:clauseT(RefClauseId, _, RefModule, RefFunctor, RefArity),
%%    functor(RefHead, RefFunctor, RefArity),
%%    predicate_property(RefModule:RefHead, _),
%%    clause(RefModule:RefHead, Ref),
%%	 parse_util:termT(RefLitId, RefTerm),
%%    is_reference_to(DeclModule:DefHead,RefHead,RefModule:RefTerm,Kind).
%	    
%
%is_reference_to(DefModule:DefSignature, ExactMatch, RefHead, Reference, RefKind) :-
%    ( Reference = RefModule:RefTerm
%    -> is_reference_to__(DefModule,DefSignature, ExactMatch, RefHead, RefModule, RefTerm,   RefKind)
%    ;  is_reference_to__(DefModule,DefSignature, ExactMatch, RefHead, _,         Reference, RefKind)
%    ).
%
%is_reference_to__(DefModule,DefFunctor/DefArity, ExactMatch, RefHead, RefModule, RefTerm, RefKind) :-
%	nonvar(DefModule),
%	var(DefFunctor),
%    nonvar(RefTerm),
%    !, % Reference to module
%    (	ExactMatch == true
%    ->	DefModule == RefModule
%    ;	nonvar(RefModule),
%    	once(sub_atom(RefModule, _, _, _, DefModule))
%    ),
%    ref_kind(DefModule, DefFunctor/DefArity, RefHead, RefModule,  RefKind).
%
%is_reference_to__(DefModule,DefFunctor/DefArity, ExactMatch, RefHead, RefModule, RefTerm, RefKind) :- 
%    nonvar(DefFunctor),
%    nonvar(RefTerm),
%    functor(RefTerm, RefFunctor, RefArity),
%    (	ExactMatch == true
%    ->	DefFunctor == RefFunctor
%    ;	once(sub_atom(RefFunctor, _, _, _, DefFunctor))
%    ),
%    (	(var(DefArity); DefArity == -1)
%    ->	true
%    ;	DefArity == RefArity
%    ),
%%    DefTerm=RefTerm,  % It is a reference! Determine its kind:
%    ref_kind(DefModule, DefFunctor/DefArity, RefHead, RefModule,  RefKind).
%
%ref_kind(DefModule, _, _, RefModule, RefKind) :-     
%    DefModule == RefModule,
%    !,
%    RefKind = call.
%ref_kind(_, _, '$mode'(_, _), _, RefKind) :- 
%    !,
%    RefKind=prologdoc.   
%ref_kind(_, _, _, _, RefKind) :- 
%    RefKind=termORmetacall.
             

%               /********************************************
%                * FIND REFERENCES TO A PREDICATE DEFINITON *
%                ********************************************/
%       Version von Toias, die auf Paarsen des Outputs von "explain" basierte
%       (war langsamer, fand weniger Referenzen (keine Metaterme) und war 
%       nicht in der Lage, die Art der Referenzen (Call, Metacall, PrologDoc)
%       zu unterscheiden. "expalin" bot f�r all das keinen Ansatzpunkt 
%
%%% get_references(+EnclFile,+PredSignature,?Module, -FileName,-Line,-RefModule,-Name,-Arity) is nondet.
%%
%%  @param PredSignature PredName/Arity
%%  @author TRHO
%%
%get_references(EnclFile, Name/Arity,Module, RefFile,Line,RefModule,RefName,RefArity):-
%    (  atom(Module)
%    -> true                              % Explicit module qualification
%    ;  module_of_file(EnclFile,Module)   % Implicit module qualification
%    ),
%    functor(Pred,Name,Arity),            % Predicate to be searched 
%    % INTERNAL, works for swi 5.11.X
%    prolog_explain:explain_predicate(Module:Pred,Explanation), 
%%    writeln(Explanation),
%    decode_reference(Explanation,Nth, RefModule,RefName, RefArity),
%    number(RefArity),
%    defined_in_file(RefModule,RefName,RefArity,Nth,RefFile,Line).
%%   <-- Extracted predicate for:
%%    nth_clause(RefModule:Head,Nth,Ref),
%%    clause_property(Ref,file(FileName)),
%%    clause_property(Ref,line_count(Line)).
%
%      
%
%%% decode_reference(+RefStr,-Nth, -RefModule, +Pred,-Arity) is nondet.
%%
%% Reference string from explain/2 predicate
%% 
%%  IMPORTANT: Hardcoded reference to the user module!
%%  Only works for predicates defined in the user module!
%%
%decode_reference(RefStr,Nth, RefModule,Pred,Arity):-
%    atom_concat('        Referenced from ',Rest,RefStr),
%    atom_concat(NthAtom,Help0,Rest),
%    atom_concat('-th clause of ',PredRef,Help0),
%    atom_concat(RefModule,Help1,PredRef),
%    atom_concat(':',PredicateIndicator,Help1),
%    atom_concat(Pred,Help2,PredicateIndicator),
%    atom_concat('/',ArityAtom,Help2),
%    atom_number(NthAtom,Nth),
%    atom_number(ArityAtom,Arity),
%    !.
%
%%%%%%%%%%% Tests %%%%%%%%%%%
%
%user:setUp(decode_reference) :-
%	assert(user:testpred(1,2)).
%user:test(decode_reference) :-
%    decode_reference('        Referenced from 1-th clause of user:testpred/2',
%                     1, 'testpred',2).
%
%user:tearDown(decode_reference) :-
%	retract(user:testpred(1,2)).


