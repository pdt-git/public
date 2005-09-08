% Author: Uwe Bardey, Günter Kniesel
% Date: 04.11.02

/* Commented out since SWI handles modules in a slightly unintuitive way. Left
   here just for documentation purposes:
   
:- module(dep_graph, [

   set_condor_default_options/0,      % logging+all_orders+no_self-dependencies 
    enable_logging/0,
   disable_logging/0,
    enable_all_orders/0,              % compute all conflict-free orders
   disable_all_orders/0,              % compute only one conflict-free order
    enable_reflexive_dependencies/0,
   disable_reflexive_dependencies/0,

   % The predicates above this line should be used to set global options  
   % that will affect all subsequent analyses. The ones below are for running
   % the analyses.
   
   dep_analysis_interactive/1,        % +Ctlist: output on console
   dep_analysis_batch/2               % +Filename,+CTlist: file output  
   ]).
*//*
   Internal only:

   dep_analysis(_stream, _ctlist)     % do it all (main internal entry point) 
   gen_dep_graph(_stream, _ctlist)    % generate dep graph facts (and report)
   order_dep_graph(_stream, _ctlist)  % generate order facts (and report)
   ... and others ...
*/   


/**
 * Helper predicates for setting of global options that control the 
 * behaviour of the analysis algorithm: 
 *  - logging
 *  - creation of one or all orders / conflicts
 *  - inclusion of reflexive dependencies into analysis
 */   
set_condor_default_options :-
    enable_logging,                 % default: logging
    enable_all_orders,              % default: compute all non-conflicting orders
    disable_reflexive_dependencies. % default: don't compute self-dependencies
    
     
:- dynamic loggingEnabled/0.
loggingEnabled.                     % default: logging

enable_logging :- 
   retractall(loggingEnabled),      % prevent accidental backtracking(!)
   assert(loggingEnabled).
   
disable_logging :- 
   retractall(loggingEnabled).
   
   
:- dynamic gen_order_option/1.
gen_order_option(all).             % default: compute all non-conflicting orders

enable_all_orders :- 
   retractall(gen_order_option(_)),
   assert(gen_order_option(all)).
   
disable_all_orders :- 
   retractall(gen_order_option(_)),
   assert(gen_order_option(one)).
 
 
:- dynamic dependency_option/1.
dependency_option(non_reflexive).  % default: don't compute self-dependencies
   
enable_reflexive_dependencies :-
   retractall(dependency_option(_)),
   assert(dependency_option(reflexive)).
   
disable_reflexive_dependencies :- 
   retractall(dependency_option(_)),
   assert(dependency_option(non_reflexive)).
   

/** **************************************************************************
 * Dependency analysis: Top-level predicates (public / for external use).
 */ 
    
dep_analysis_interactive(_ctlist) :-
    current_output(_stream),
    !,
    dep_analysis(_stream, _ctlist).
    
dep_analysis_batch(_filename, _ctlist) :-
    open(_filename, write, _stream),
    !,
    dep_analysis(_stream, _ctlist),
    close(_stream).

/* ***************************************************************************
 *  All predicates below this line are private to the dependency analysis.
 */ 

% Cache computation results
:- dynamic use_cache_results_only/0.
:- dynamic ct_node/2.
:- dynamic ct_edge/5.
:- dynamic ct_order/2.
:- dynamic ct_conflict/2.
:- dynamic ct_circle/2.
          
/**
 * Clear flags and caches used by the dependency analysis.
 * Do not clear the global options that are valid across multiple
 * analysis runs (le.g. logging and number of orders).
 */     
del_dep_graph :-
    retractall(use_cache_results_only),
    retractall(ct_node(_,_)),
    retractall(ct_edge(_,_,_,_,_)),
    retractall(ct_order(_,_)),
    retractall(ct_conflict(_,_)),
    retractall(ct_circle(_,_)),
    format('~ndel_dep_graph/0 completed.~n',[]).
    
/**
 * dep_analysis(+stream, +ctlist)
 *
 * Create dependency graph, find topological order or cycles and print results:
 */
dep_analysis(_stream, _ctlist) :-
    del_dep_graph,                 % Prevent interferences from previous runs
      startStopwatch,              % start prolog's timer 
      term_to_atom(_ctlist,CTs),
      log(_stream, '~n --- Starting gen_dep_graph(stream,~a).~n~n',[CTs]),
    gen_dep_graph(_stream, _ctlist),
      reportRuntime(' --- Constructing and printing the dependency graph took'),
    order_dep_graph(_stream, _ctlist),
      reportRuntime(' --- Identifying all orders, cycles and conflicts took'),
      log(_stream, '~nThat\'s it, folks... ~n~n',[]).

/**
 * gen_dep_graph(+stream, +ctlist)
 * 
 * Create the dependency graph for the CTs whose head literals are elements of
 * arg2. Sho logging output on stream arg1 if logging is enabled.
 */    
gen_dep_graph(_stream, _ctlist) :-
    % Assert and print ct_node/2 facts:
    gen_dep_nodes(_stream, _ctlist),
    % Assert and print ct_edge/4 facts for positive dependencies:
    gen_dep_edges(_stream, _ctlist, positive),
    % Assert and print ct_edge/4 facts for negative dependencies:
    gen_dep_edges(_stream, _ctlist, negative).
 
 
/**
 * gen_dep_nodes(+stream, +ctlist)
 *
 * Assert and print ct_node(Id,Label) facts. Every CTHead element from the  
 * CT list in arg2 produces a fact with Label=CTHead. The Id is generated
 * automatically to be globally unique within Condor.
 */
gen_dep_nodes(_stream, _ctlist) :-
      length(_ctlist, _total),
      log(_stream,'~n --- Dependency graph with ~a nodes: ~n~n',[_total]),
    findall(_ct, 
            ( member(_ct, _ctlist),
              ct(_ct,_,_),
              generate_and_log(_stream, ct_node(_ct)) % <-- the identity argument
            ),                                        % of ct_node/2 is added 
            _ctl1).                                   % by generate_and_log!
 
/**
 * gen_dep_edges(+stream, +ctlist, +type)
 *
 * Find all dependencies of type arg3 for the CTs in arg2, sort them, 
 * eliminate duplicates and finally store them and log it.
 */
gen_dep_edges(_stream, _ctlist, _type) :-
    findall((_ct1,_ct2,_label,_type), 
            ( member(_ct1, _ctlist),
              member(_ct2, _ctlist),
              depend(_ct1, _ct2, _type, _label)
            ),
            _allDependencyPairs
    ),
    sort(_allDependencyPairs, _sorted ),
    % *** See comment after end of this clause ***
      length(_sorted, _total),
      log(_stream,'~n --- Found ~a dependencies (total = ~a): ~n~n',[_type, _total]),
    forall( member((_ct1,_ct2,_label,_type),_sorted),  % _sortedUniqueDependencyPairs
            generate_and_log(_stream, ct_edge(_ct1,_ct2,_label,_type))
   ).                                 % <-- the identity argument of ct_edge/5 
                                      % is added by generate_and_log!


    % This comment should be read as if placed in the previous clause in the
    % line marked with ***.
    %
    % The following five lines replace multiple unifiable dependency facts by 
    % their unified version. I found that to be harder to interprete, so the 
    % current version does not contain this feature:
    %
    % removeDuplicates(_sorted, _sortedUniqueDependencyPairs),
    % length(_allDependencyPairs, _total),
    % length(_sortedUniqueDependencyPairs, _unique),
    % log(_stream,'~n --- Found ~a dependencies: total = ~a, unique = ~a. ',
    %     [_type, _total, _unique]),
    % log(_stream,'Listing of nonredundant, ~a dependencies: ~n~n',[_type]),
    %
    % To re-enable the above feature replace the following two lines by the 
    % five lines above and replace "_sorted" by "_sortedUniqueDependencyPairs" 
    % in the member/2 invocation below:
    %
    % This comment should be read as if placed in the previous clause of
    % gen_dep_edges/3 in the line marked with ***.


/**
 * order_dep_graph(+stream, +ctlist)
 *
 * Compute and report order or cycles and conflicts.
 */
order_dep_graph(_stream, _ctlist) :-
    assert(use_cache_results_only),
    gen_cycles(_stream, _ctlist,Problems),
    gen_order_option(_oneOrAll),
    ( (Problems > 0) -> true 
      ; gen_order(_oneOrAll,_stream, _ctlist) ).

/**
 * Find all cycles, categorize them by their type and assert them in the
 * analysisi database. Return in arg3 the number of problematic cycles
 * (that is all cycles that are not self-inhibition cycles (that is 
 * negative cycles of lenght 1).
 */
gen_cycles(_stream, _ctlist, Problems) :-
    find_all_cycles_by_type(Negative,Positive,Mixed),
    findall( X, (member(X,Negative), length(X,1)), Benign),
    nl,
    ( (Benign = []) -> true 
       ; shorten_and_generate('inhibition cycles (benign)', 
       ct_cycle_inhibition_ok, Benign, _stream) ),   
    ( (Conflicts = []) -> true 
     ; shorten_and_generate('inhibition cycles (malign)', 
       ct_cycle_inhibition_bad, Conflicts, _stream) ),
    ( (Positive = []) -> true 
     ; shorten_and_generate('triggering cycles', 
       ct_cycle_triggering, Positive, _stream) ),
    ( (Positive = []) -> true 
     ; shorten_and_generate('mixed cycles', ct_cycle_mixed, Mixed, _stream) ),
    length(Benign,B),
    length(Negative,N),
    length(Positive,P),
    length(Mixed,M),    
    Problems is M+P+N-B.


/**
 * gen_order(+oneOrAll,+stream, +ctlist)
 *
 * Depending on the value of the first argument generate either one sample
 * order or all possible topological orders for the dependency graph whose 
 * nodes are specified in arg3.
 */  

% Generate (assert and print) one possible order:
gen_order(one, _stream, _ctlist) :-
    once(topo_sort(_ctlist, _order)),
      log(_stream, '~n --- Sample conflict-free order (ther might be others):~n~n',[]),
    generate_and_log(_stream, ct_order(_order)).  % <-- the identity argument
                                                  % of ct_node/2 is added 
                                                  % by generate_and_log!

% Generate (assert and print) all possible orders:
gen_order(all, _stream, _ctlist) :-
    findall(_order, topo_sort(_ctlist, _order), Orders ),
    shorten_and_generate('conflict-free orders', ct_order, Orders, _stream ).

/**
 * shorten_and_generate(+What, +Functor, +List, +Stream)
 *
 * Arg3 is a List of elements that should be stored as a set of facts with 
 * Functor arg2, that is, for every Element we store "Functor(Element)". 
 * In addition, every stored fact is logged on the output stream arg4 
 * using the message Arg1. If there are too many elements, the user is 
 * asked (in interactive mode) whether logging should be disabled.
 */
shorten_and_generate(_what, Functor, List, _stream) :-      
      length(List, _total),
    sort(List,Sorted),   % this should also remove duplicates but it doesn't
    removeDuplicates(Sorted,SortUnique),
      length(SortUnique, _uniqueNr),
      log(_stream, '~n --- Found ~a: total = ~a, unique = ~a:~n~n', 
          [_what, _total, _uniqueNr]),
      switchOffLoggingIfDesired(_uniqueNr,_what),
    forall( ( member(Elem,SortUnique), 
              Fact=..[Functor,Elem] 
             ),
             generate_and_log(_stream, Fact)
          ).  

removeDuplicates([First|Rest],NoDup) :-
   member(First,Rest),
   !,
   removeDuplicates(Rest,NoDup).
removeDuplicates([First|Rest],[First|NoDup]) :-
   removeDuplicates(Rest,NoDup).
removeDuplicates([],[]).



switchOffLoggingIfDesired(_uniqueNr,_what) :-  % Don't ask if < 100 results.
      _uniqueNr <100,
      !.
switchOffLoggingIfDesired(_uniqueNr,_what) :-  % Ask whether to disable logging.
      loggingEnabled,
      !,
      log(_stream,'~n --- Printing ~a different ~a might take a while... ', 
          [_uniqueNr, _what]),
      log('Skip logging for the rest of this analysis? [yes. / no.]:~n', 
          []),
      read(Answer),
      ( ( Answer='yes') -> 
         (log(_stream, 'Logging disabled...~n', []), disable_logging)
        ; true ).
switchOffLoggingIfDesired(_uniqueNr,_what) .  % Go on logging if in batch mode.

   
/**
 * Helper predicates for asserting facts into dependency analysis cache 
 * and for reporting these actions if logging is enabled.
 */
generate_and_log(_stream, Term) :-
     Term =.. [Functor|Args],
     next_condor_fact_id(Id),
     TermWithId =.. [Functor, Id | Args], %**** Test this !!!
     assert(TermWithId),
     term_to_atom(TermWithId, Atom),
     log(_stream, '~a~n',Atom).

log(Stream,Formatterm,Atomlist) :-
    (loggingEnabled -> format(Stream,Formatterm,Atomlist) ; true ).    


:- dynamic(condor_fact_id/1);

condor_fact_id(0).

next_condor_fact_id(Y) :- 
    retract(condor_fact_id(X)),
    !,
    retractall(condor_fact_id(_)),
    Y is X+1,
    assert(condor_fact_id(Y)).
next_condor_fact_id(1) :-
    assert(condor_fact_id(1)).
    
reset_condor_fact_id :-
    retractall(condor_fact_id(_)),
    assert(condor_fact_id(0)).    
/**
 * Helper predicates for reporting time spent.
 *   - startStopwatch
 *   - reportRuntime(ForWhat)
 */
startStopwatch :-
    statistics(runtime, _CPU),    % Start new CPU timer
    statistics(real_time, _Real). % Start new real timer
    
reportRuntime(ForWhat) :- 
    statistics(runtime,    [_CPUMilisSinceStart, CPUMilisSinceLast]),   
    statistics(real_time,  [_RealSecsSinceStart, RealSecsSinceLast]), 
    format('~n~a: CPU = ~a milliseconds, real time ca. ~a seconds~n', 
           [ForWhat,CPUMilisSinceLast,RealSecsSinceLast]).
    
      
