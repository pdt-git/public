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
         [ find_reference_to/12 % +Functor,+Arity,?DefFile,?DefModule,
                                % ?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind,?PropertyList
         ]
         ).

:- use_module(pdt_prolog_library(utils4modules)).

:- use_module( properties, [properties_for_predicate/4] ).
:- use_module(library(lists)).

%:- ensure_loaded('../pdt_factbase.pl').
%:- use_module('../modules_and_visibility').
    /*********************************************
     * FIND REFERENCES TO A PARTICULAR PREDICATE *
     ********************************************/
find_unique( Goal ) :-
    setof( Goal, Goal, Set),
    member(Goal, Set).
    
%% find_reference_to(+Functor,+Arity,DefFile, DefModule,RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,?PropertyList)
find_reference_to(Functor,Arity,DefFile, SearchMod,
                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) :-
    find_unique(  find_reference_to__(Functor,Arity,DefFile, SearchMod,
                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) ).
    
find_reference_to__(Functor,Arity,DefFile, SearchMod,
                  RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) :-                  
	( nonvar(DefFile)
    -> module_of_file(DefFile,SearchMod)
    ; true % Defining File and defining Module remain free ==> 
           % Search for references to independent definitions
           % <-- Does that make sense???
    ),
    ( var(Arity) % Need to backtrack over all declared Functor/Arity combinations:
    -> ( setof( Functor/Arity, SearchMod^current_predicate(SearchMod:Functor/Arity), Set),
         member(Functor/Arity, Set)
       )
    ; true % Arity already bound in input
    ),
    functor(SearchTerm,Functor,Arity),
    pdt_xref_data(SearchMod:SearchTerm,RefModule:RefHead,Ref,Kind),

    functor(RefHead,RefName,RefArity),
    predicate_property(RefModule:RefHead,_),
    nth_clause(RefModule:RefHead,Nth,Ref),
    clause_property(Ref, file(RefFile)),
    clause_property(Ref, line_count(RefLine)),
    properties_for_predicate(RefModule,RefName,RefArity,PropertyList).

go :- % To list all results quickly call 
      % ?- pdt_xref:go, fail.
    find_reference_to(defined_in_file,6,__DefFile, __DefModule,RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,_PropertyList),
    format( '~a reference from ~a:~w clause ~a, line ~a, file ~n~a~n~n',
            [Kind, RefModule,RefName,RefArity, Nth, RefLine, RefFile]
    ).
	

pdt_xref_data(DefModule:T,RefModule:RefHead,Ref, Kind) :-
   current_predicate(RefModule:F/A),     % For all defined predicates
   functor(RefHead,F,A),   
   nth_clause(RefModule:RefHead,_N,Ref),   % For all their clauses
   '$xr_member'(Ref, QualifiedTerm),					% Get a term referenced by that clause
   is_reference_to(DefModule:T,RefHead,QualifiedTerm,Kind).     % (via SWI-Prolog's internal xref data)
 

   
%pdt_xref_data(DefModule:DefHead,RefModule:RefHead,Ref, Kind) :-
%    functor(DefHead, DefFunctor, DefArity),
%    modules_and_visibility:get_predicate_referenced_as(DefModule, DefFunctor, DefArity, DefId),
%    (	DefId = predefined(DeclModule, Functor, Arity)
%    ->	parse_util:call_built_in(Functor, Arity, DeclModule, RefLitId)
%    ;	parse_util:call_edge(DefId, RefLitId)
%    ),
%    parse_util:literalT(RefLitId, _, RefClauseId, _, _, _),
%    parse_util:clauseT(RefClauseId, _, RefModule, RefFunctor, RefArity),
%    functor(RefHead, RefFunctor, RefArity),
%    predicate_property(RefModule:RefHead, _),
%    clause(RefModule:RefHead, Ref),
%	 parse_util:termT(RefLitId, RefTerm),
%    is_reference_to(DeclModule:DefHead,RefHead,RefModule:RefTerm,Kind).
	    

is_reference_to(DefModule:DefTerm, RefHead, Reference, RefKind) :-
    ( Reference = RefModule:RefTerm
    -> is_reference_to__(DefModule,DefTerm, RefHead, RefModule, RefTerm, RefKind)
    ;  is_reference_to__(DefModule,DefTerm, RefHead, _______, Reference, RefKind)
    ).

is_reference_to__(DefModule,DefTerm, RefHead, RefModule, RefTerm, RefKind) :- 
    nonvar(DefTerm),
    nonvar(RefTerm),
    DefTerm=RefTerm,  % It is a reference! Determine its kind:
    ref_kind(DefModule, DefTerm, RefHead, RefModule,  RefKind).

ref_kind(DefModule, _, _, RefModule, RefKind) :-     
    DefModule == RefModule,
    !,
    RefKind = call.
ref_kind(_, _, '$mode'(_, _), _, RefKind) :- 
    !,
    RefKind=prologdoc.   
ref_kind(_, _, _, _, RefKind) :- 
    RefKind=termORmetacall.
             

%               /********************************************
%                * FIND REFERENCES TO A PREDICATE DEFINITON *
%                ********************************************/
%       Version von Toias, die auf Paarsen des Outputs von "explain" basierte
%       (war langsamer, fand weniger Referenzen (keine Metaterme) und war 
%       nicht in der Lage, die Art der Referenzen (Call, Metacall, PrologDoc)
%       zu unterscheiden. "expalin" bot für all das keinen Ansatzpunkt 
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


