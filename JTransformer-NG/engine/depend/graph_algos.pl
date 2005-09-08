% Autor: Günter Kniesel
% Date: August 28, 2005
% Replaced Uwe's naive (super-exponential) initial implementation of cycle and 
% conflict detection by an algorithm with less than quadratic worst case
% complexity in the number of edges of the dependency graph.  

% Known uses: Public predicate called from dep_graph.pl::gen_order/2

:- module(condor_graph_algos, [ find_all_cycles_by_type/3 
   /* 
        % Only make these publicly visible when debugging:                  
          visit_graph/1,  % Return one cycle. Backtrack to find all.
          visit_outgoing_edges/4,
          check_cycle/4,
          path_member/4,
          copy_condor_dep_graph/0,
          list_copied_dep_graph/0,
          delete_copied_dep_graph/0
  */
]).

/******************************************************************************/

/**
 *  find_all_cycles_by_type(?Neg,?Pos,?Mixed)
 *    
 *  Return three lists of minimal cycles in the dependency graph:
 *  - arg1 = purely negative (inhibition) cycles,
 *  - arg2 = purely positive (triggering) cycles, 
 *  - arg3 = mixed cycles.
 */
 find_all_cycles_by_type(Negative,Positive,Mixed) :-
     delete_copied_dep_graph,
     copy_condor_dep_graph,
     findall(cyc(Type,Cycle), find_cycle(Type,Cycle), AllCycles),
     findall(Neg, member(cyc(negative,Neg),AllCycles), Negative),
     findall(Pos, member(cyc(positive,Pos),AllCycles), Positive),
     findall(Mix, member(cyc(mixed,Mix),AllCycles),  Mixed),
     delete_visited_markers,
     !.
 find_all_cycles_by_type([],[],[]) :-
     delete_visited_markers.
          
/******************************************************************************/
      
:- dynamic condor_graph_algos:ct_node/2.
     
:- dynamic condor_graph_algos:ct_edge/5.

/**
 * copy_condor_dep_graph/0
 * 
 * Create a local copy of the dependency graph. 
 */
copy_condor_dep_graph :-                             % nodes 
    clause(               user:ct_node(Id,Label),_ ),
    assert( condor_graph_algos:ct_node(Id,Label)   ),
    fail.
copy_condor_dep_graph :-                             % edges 
    clause(               user:ct_edge(Id,From,To,Label,Type),_ ),
    assert( condor_graph_algos:ct_edge(Id,From,To,Label,Type)   ),
    fail.
copy_condor_dep_graph . 
    % list_copied_dep_graph.

list_copied_dep_graph :-
    current_output(_Stream),
    % log(_Stream,' --- Condor graph analysis: Copyied Graph:',[]),
    write(' --- Condor graph analysis: Copyied Graph:'),
    listing( condor_graph_algos:ct_node ),
    listing( condor_graph_algos:ct_edge ),
    % log(_Stream,' --- Condor graph analysis: Copyied Graph listing completed',[]).
    write(' --- Condor graph analysis: Copyied Graph listing completed').

delete_copied_dep_graph :-
   retractall(condor_graph_algos:ct_node(_,_)),
   retractall(condor_graph_algos:ct_edge(_Id,_From,_To,_Label,_Type)). 

/******************************************************************************/
 
/**
 * find_cycle(?Type,?Cycle) 
 *
 * Analyse the local copy of the dependency graph stored in ct_node and
 * ct_edge facts an return in arg2 a cycles represented  
 * by a list of edge identities in reverse order (edges at the front of  
 * the list are preceded by edges deeper in the list). 
 */   
find_cycle(Type,Cycle) :-
      nl, write(' --- Visiting graph: '),nl,
   ct_node(Id,Label),  
   not( visited(Id) ),
   mark_visited( Id ),
      nl, write(' |-- '), write(ct_node(Id,Label)),
   visit_outgoing_edges(Label,[Id],[],ReverseCycle),
   reverse(ReverseCycle,Cycle),
   determine_type(Type,Cycle).

   
determine_type(positive,Cycle) :-
   forall( member(Id,Cycle), ct_edge(Id,_,_,_,positive) ),
   !.
determine_type(negative,Cycle) :-
   forall( member(Id,Cycle), ct_edge(Id,_,_,_,negative) ),
   !.
determine_type(mixed,_Cycle) .    

/**
 * visit_outgoing_edges(+From,+RevVisitedNodes,+RevVisitedEdges,?Cycle)
 *
 *
 */
visit_outgoing_edges(From,RevVisitedNodes,RevVisitedEdges,Cycle) :-
   ct_edge(Id,From,To,L,Type),
      write(' --> '), write(ct_edge(Id,From,To,L,Type)),
   check_cycle(To,RevVisitedNodes,[Id|RevVisitedEdges],Cycle).


/**
 * check_cycle(+NodeLabel,+RevVisitedNodes,+RevVisitedEdges,+PrevCycles,?Cycles)
 *
 * If the id of node arg1 is in the list of visited nodes (= arg2) then 
 * return in arg5 the extension of the list of previous cycles (arg4) by the  
 * newly detected cycle that is a prefix of the list of visited edges (arg3). 
 */
check_cycle(To,RevVisitedNodes,RevVisitedEdges,Cycle) :-
   once(ct_node(Id,To)),
   path_member(Id,RevVisitedNodes,RevVisitedEdges,Cycle),
      nl, write(' ==> '), write(cycle(Cycle)), nl.
  
 
check_cycle(To,RevVisitedNodes,RevVisitedEdges,Cycle) :-
   once(ct_node(Id,To)),
   not( visited(Id) ),
   mark_visited( Id ),
     nl, write(' --> '), write(ct_node(Id,To)),
  visit_outgoing_edges(To,[Id|RevVisitedNodes],RevVisitedEdges,Cycle).
 

/**
 * path_member(Id, NIds, EIds, Eids)
 * 
 * arg1 is a node identity. 
 * arg2 is a list of already visited node identities, the latest at the top.
 * arg3 is a list of already visited edge identities, the latest at the top.
 * arg2 and arg3 are lists of same length obeying the invariant that 
 * the n-th element of arg2 is the identity of a node that is the source of 
 * the edge that is the n-th element of arg3. The edges in arg4 form a path
 * from the first node to the one passed as arg1.
 *
 * Here we only need to know that we must find out which element of arg2 is
 * arg1 and then we want to return the corresponding prefix of arg3 (which is
 * a cyclic path from arg1 to itself).
 *
 * Tests:
%?- condor_graph_algos:path_member(a, [c,b,a,z],[3,2,1,24],[3,2,1]).--> yes 
%?- condor_graph_algos:path_member(a, [a,z],[1,24],[1]).            --> yes
%?- condor_graph_algos:path_member(f, [c,b,a,z],[3,2,1,24],Cycle).  --> no
%?- condor_graph_algos:path_member(_, [],_,_).                --> exception
%?- condor_graph_algos:path_member(_, _,[],_).                --> exception
%?- condor_graph_algos:path_member(a, [1,2],[a,b,c],_).       --> exception         
 */ 
path_member(Id,L1,L2,L3) :-
  nonvar(Id),
  nonvar(L1), length(L1,L),
  nonvar(L2), length(L2,L),
  !,
  path_member_(Id,L1,L2,L3). 
path_member(Id,L1,L2,L3) :-
  throw( preconditons_violated_for_arguments_of_path_member(Id,L1,L2,L3) ).
  
path_member_(Id,[Id     ],[EdgeId  ],[EdgeId  ]) :-    % self cycle
   !.  
path_member_(Id,[Id|_   ],[EdgeId|_],[EdgeId  ]) :-  % end of cycle
   !.  
path_member_(Id,[_|Nodes],[EId|Rest ],[EId|Cycle]) :-
   path_member_(Id,Nodes,Rest,Cycle).  

   
/**
 * Mark node as visited.   
 */
:- dynamic(visited/1).

mark_visited(Id) :- 
	assert( visited(Id) ).

delete_visited_markers :-
    retractall(visited(_)).
    
/******************************************************************************/
    
/* test-graph
ct_node(1000001,a ).
ct_node(1000002,b ).
ct_node(1000003,c ).
ct_node(1000004,d1).
ct_node(1000005,d2).
ct_node(1000006,d3).
ct_node(1000007,d4).
ct_node(1000008,e ).
ct_node(1000009,f ).

ct_node(10000010,z ).
ct_node(10000011,y ).
ct_node(10000012,x1).
ct_node(10000013,w ).
ct_node(10000014,v ).
ct_node(10000015,u ).
ct_node(10000016,t ).

     
ct_edge(100056, a,  b, incoming, positive).
ct_edge(100057, b, c,  incoming, positive).
ct_edge(100058, c, d1, incoming, positive).
ct_edge(100059, d1,d2, c1_12,    positive). % indirect cycle, positive
ct_edge(100060, d2,d3, c1_23,    positive). % indirect cycle, positive
ct_edge(100061, d3,d4, c1_34,    positive). % indirect cycle, positive
ct_edge(100062, d4,d2, c1_42,    positive). % indirect cycle, positive
ct_edge(100063, d4,d1, c1_41,    negative). % overlapping indirect cycle, mixed
ct_edge(100064, d1, e, outgoing, positive).
ct_edge(100064, d4, f, outgoing, positive).
ct_edge(100064, d4, f, outgoing, positive).

ct_edge(200056, z, y,  incoming, positive).
ct_edge(200057, y, x1, incoming, positive).
ct_edge(200058, x1,x1, x1_x1,    negative). % self-cycle, negative
ct_edge(200059, x1,w,  outging,  positive).
ct_edge(200060, w, v,  outging,  positive).
ct_edge(200061, v, u,  outgoing, positive).
ct_edge(200062, y, u,  nocycle,  negative).
ct_edge(200063, z, t,  nocycle,  negative).
ct_edge(200064, u, t,  outgoing, positive).

*/
/*
For the above facts this is the expected output:

visit_graph(Cycle).

 --- Visiting graph: 

 |-- ct_node(1000001, a) --> ct_edge(100056, a, b, _G239, _G240)
 --> ct_node(1000002, b) --> ct_edge(100057, b, c, _G261, _G262)
 --> ct_node(1000003, c) --> ct_edge(100058, c, d1, _G283, _G284)
 --> ct_node(1000004, d1) --> ct_edge(100059, d1, d2, _G305, _G306)
 --> ct_node(1000005, d2) --> ct_edge(100060, d2, d3, _G327, _G328)
 --> ct_node(1000006, d3) --> ct_edge(100061, d3, d4, _G349, _G350)
 --> ct_node(1000007, d4) --> ct_edge(100062, d4, d2, _G371, _G372)
 ==> cycle([100062, 100061, 100060])

Cycle = [100062, 100061, 100060] ;
 --> ct_edge(100063, d4, d1, _G371, _G372)
 ==> cycle([100063, 100061, 100060, 100059])

Cycle = [100063, 100061, 100060, 100059] ;
 --> ct_edge(100064, d4, f, _G371, _G372)
 --> ct_node(1000009, f) --> ct_edge(100064, d4, f, _G371, _G372) --> ct_edge(100064, d1, e, _G305, _G306)
 --> ct_node(1000008, e)
 |-- ct_node(10000010, z) --> ct_edge(200056, z, y, _G239, _G240)
 --> ct_node(10000011, y) --> ct_edge(200057, y, x1, _G261, _G262)
 --> ct_node(10000012, x1) --> ct_edge(200058, x1, x1, _G283, _G284)
 ==> cycle([200058])

Cycle = [200058] ;
 --> ct_edge(200059, x1, w, _G283, _G284)
 --> ct_node(10000013, w) --> ct_edge(200060, w, v, _G305, _G306)
 --> ct_node(10000014, v) --> ct_edge(200061, v, u, _G327, _G328)
 --> ct_node(10000015, u) --> ct_edge(200064, u, t, _G349, _G350)
 --> ct_node(10000016, t) --> ct_edge(200062, y, u, _G261, _G262) --> ct_edge(200063, z, t, _G239, _G240)

No

*/
    