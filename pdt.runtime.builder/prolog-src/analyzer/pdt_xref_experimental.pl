:- module( pdt_xref,
         [ find_reference_to/12 % +Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind
         ]
         ).

:- use_module(org_cs3_lp_utils(utils4modules)).
:- use_module(properties).
%:- use_module('../parse_util').
    /*********************************************
     * FIND REFERENCES TO A PARTICULAR PREDICATE *
     ********************************************/

%% find_reference_to(+Functor,+Arity,DefFile, DefModule,RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,?PropertyList)
find_reference_to(Functor,Arity,DefFile, DefModule,RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList) :-
	( nonvar(DefFile)
    -> module_of_file(DefFile,DefModule)
    ; true % Defining File and defining Module remain free ==> 
           % Search for references to independent definitions
           % <-- Does that make sense???
    ),
    ( var(Arity) % Need to backtrack over all declared Functor/Arity combinations:
    -> ( setof( Functor/Arity, DefModule^current_predicate(DefModule:Functor/Arity), Set),
         member(Functor/Arity, Set)
       )
    ; true % Arity already bound in input
    ),
    functor(T,Functor,Arity),
    pdt_xref_data(DefModule:T,RefModule:RefHead,Ref,Kind),
    functor(RefHead,RefName,RefArity),
    predicate_property(RefModule:RefHead,_),
    nth_clause(RefModule:RefHead,Nth,Ref),
    clause_property(Ref, file(RefFile)),
    clause_property(Ref, line_count(RefLine)),
    properties_for_predicate(RefModule,RefName,RefArity,PropertyList).

go :- % To list all results quickly call 
      % ?- pdt_xref:go, fail.
    find_reference_to(defined_in_file,6,__DefFile, __DefModule,RefModule,RefHead,RefFile,RefLine,Nth,Kind,_PropertyList),
    format( '~a reference from ~a:~w clause ~a, line ~a, file ~n~a~n~n',
            [Kind, RefModule,RefHead, Nth, RefLine, RefFile]
    ).



pdt_xref_data(DefModule:T,RefModule:RefHead,Ref, Kind) :-
   current_predicate(RefModule:F/A),     % For all defined predicates
   functor(RefHead,F,A),   
   nth_clause(RefModule:RefHead,_N,Ref),   % For all their clauses
   '$xr_member'(Ref, X),					% Get a term referenced by that clause
   extractModule(X,T,DefModule,RefHead,Kind).     % (via SWI-Prolog's internal xref data)
   
%pdt_xref_data(DefModule:DefHead,RefModule:RefHead,Ref, Kind) :-
%    functor(DefHead, DefFunctor, DefArity),
%    parse_util:predicateT_ri(DefFunctor, DefArity, DefModule, DefId),
%    parse_util:call_edge(DefId, RefLitId),
%    parse_util:literaT(RefLitId, _, RefClauseId, _, _, _),
%    parse_util:clauseT(RefClauseId, _, _, RefModule, RefFunctor, RefArity),
%    functor(RefHead, RefFunctor, RefArity),
%    predicate_property(RefHead, _),
%    clause(RefHead, Ref),
%    parse_util:termT(RefLitId, RefTerm),
%    extractModule(RefTerm,DefHead,DefModule,RefHead,Kind).


extractModule(_:T,_,_,_,_) :- 
    var(T),
    !,
    fail.
extractModule(DefModule:Term,Term,DefModule,_RefHead, call) :-
    !. 
extractModule(Module:Term,Term,DefModule,_RefHead, call) :-
    !,
    declared_in_module(Module, Term, DefiningModule),
    declared_in_module(DefModule, Term, DefiningModule).    
extractModule(Term,Term,_DefModule,'$mode'(_, _), prologdoc) :-
    !.
extractModule(Term,Term,_DefModule,_RefHead, termORmetacall) .      
                     

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
