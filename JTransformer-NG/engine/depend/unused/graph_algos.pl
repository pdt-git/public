/**
 * Public, called from dep_graph.pl::gen_order/2:
 *  - circle(+Set, CircleSortedSet)  % Cycle detection with worst case 
 *                                   % complexity 2*|Edges| + Sum(1..|Edges|)
 *  - conflict(+Set, CircleSortedSet)
 */

:- module(condor_graph_algos, [
          circle/2,  % Look within the elements of arg1 for a cycle  
                     % and return it in sorted, duplicate-free form in arg2.
          conflict/2,% Look within the elements of arg1 for a cycle consisting 
                     % of negative edges only and return it in sorted, 
                     % duplicate-free form in arg2.
    % Publicly visible just for debugging:
          copy_condor_dep_graph/0,
          list_copied_dep_graph/0,
          delete_copied_dep_graph/0,
          delete_dangling/0,
          delete_nodes_with_zero_income/0,
          delete_nodes_with_zero_outcome/0 %,
          %color_graph/0
   ]).

   
:- dynamic condor_graph_algos:ct_node/2.
     
:- dynamic condor_graph_algos:ct_edge/5.

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
copy_condor_dep_graph :- 
    list_copied_dep_graph.

list_copied_dep_graph :-
    current_output(Stream),
    log(Stream,' --- Condor graph analysis: Copyied Graph:',[]),
    listing( condor_graph_algos:ct_node ),
    listing( condor_graph_algos:ct_edge ),
    log(Stream,' --- Condor graph analysis: Copyied Graph listing completed',[]).

delete_copied_dep_graph :-
   retractall(condor_graph_algos:ct_node(_,_)),
   retractall(condor_graph_algos:ct_edge(_Id,_From,_To,_Label,_Type)). 
 
/**
 * Delete dangling  
 */ 
delete_dangling :- 
    delete_nodes_with_zero_income,
    delete_nodes_with_zero_outcome,
    current_output(Stream),
    log(Stream,' --- Copyied Graph after deleting dangling parts:',[]),
    list_copied_dep_graph.
    
/**
 * delete_nodes_with_zero_income/0
 *
 * Delete all nodes that have zero incoming edges.
 * This includes deleting all their outgoing edges.
 */
 
delete_nodes_with_zero_income :-  % delete by backtracking
    condor_graph_algos:ct_node(NodeId,Label),
    findall(_IncomingId,
            condor_graph_algos:ct_edge(_IncomingId,_,Label,_,_), 
            []), % if no incoming 
    retractall(condor_graph_algos:ct_edge(_OutgoingId,Label,_,_,_)
              ), % delete outgoing 
    retract(condor_graph_algos:ct_node(NodeId,Label)),
    current_output(Stream),
    log(Stream,' --- Deleted node ~a~n',[NodeId]),
    fail.
delete_nodes_with_zero_income :-  % if nodes remain then do the same recursively
    condor_graph_algos:ct_node(_,Label),
    findall(_IncomingId,
            condor_graph_algos:ct_edge(_IncomingId,_,Label,_,_), 
            []), % if no incoming 
    !,
    delete_nodes_with_zero_income.
delete_nodes_with_zero_income.

   
/**
 * delete_nodes_with_zero_outcome/0
 *
 * Delete all nodes that have zero outgoing edges.
 * This includes deleting all their incoming edges.
 */
delete_nodes_with_zero_outcome :- % delete by backtracking
    condor_graph_algos:ct_node(NodeId,Label),
    findall(_OutgoingId,
            condor_graph_algos:ct_edge(_OutgoingId,Label,_,_,_), 
            []), % if no outgoing 
    retractall(condor_graph_algos:ct_edge(_IncomingId,_,Label,_,_)
               ), % delete incoming 
    retract(condor_graph_algos:ct_node(NodeId,Label)),
    current_output(Stream),
    log(Stream,' --- Deleted node ~a:',[NodeId]),
    fail.
delete_nodes_with_zero_outcome :- % if nodes remain then do the same recursively
    condor_graph_algos:ct_node(_,Label),
    findall(_OutgoingId,
            condor_graph_algos:ct_edge(_OutgoingId,Label,_,_,_), 
            []), % if no outgoing 
    !,
    delete_nodes_with_zero_outcome.
delete_nodes_with_zero_outcome.



/**+**************************************/

/**
 * visit_graph(?Cycles) 
 *
 * Analyse the local copy of the dependency graph stored in ct_node and
 * ct_edge facts an return in arg1 a list of cycles. Each cycle is represented  
 * by a list of edge identities in reverse order (edges at the front of  
 * the list are preceded by edges deeper in the list). 
 */   
visit_graph(Cycles) :-
   ct_node(Id,Label),
   not( visiting(Id) ; visited(Id) ),
   mark( Id ),
   visit_outgoing_edges(Label,[Id],[],[],Cycles).

/**
 * visit_outgoing_edges(+From,+RevVisitedNodes,+RevVisitedEdges,+PrevCycles,
 *                      ?NewCycles)
 *
 *
 */
visit_outgoing_edges(From,_,_,PrevCycles,NewCycles) :-  % no unvisited outedge
   not( ( ct_edge(Id,From,_To,_,_),
          not( visiting(Id) ; visited(Id) )
      ) ),
   !,
   PrevCycles = NewCycles.
visit_outgoing_edges(From,RevVisitedNodes,RevVisitedEdges,PrevCycles,NewCycles) :-
   ct_edge(Id,From,To,_,_),
   not( visiting(Id) ; visited(Id) ),
   mark( Id ),
   check_cycle(To,RevVisitedNodes,[Id|RevVisitedEdges],PrevCycles,Cycles),
   visit_node(To,RevVisitedNodes,[Id|RevVisitedEdges],Cycles,NewCycles).


/**
 * visit_node(To,RevVisitedNodes,RevVisitedEdges,PrevCycles,NewCycles)
 *
 */
visit_node(To,_Nodes,_Edges,PrevCycles,NewCycles) :-
   not( ( ct_node(Id,To),
         not( visiting(Id) ; visited(Id) )
      ) ),
   !,
   PrevCycles = NewCycles.
   
visit_node(To,RevVisitedNodes,RevVisitedEdges,PrevCycles,NewCycles) :-
   ct_node(Id,To),
   not( visiting(Id) ; visited(Id) ),
   mark( Id ),
   visit_outgoing_edges(To,[Id|RevVisitedNodes],RevVisitedEdges,PrevCycles,NewCycles).
 

/**
 * check_cycle(+NodeLabel,+RevVisitedNodes,+RevVisitedEdges,+PrevCycles,?Cycles)
 *
 * If the id of node arg1 is in the list of visited nodes (= arg2) then 
 * return in arg5 the extension of the list of previous cycles (arg4) by the  
 * newly detected cycle that is a prefix of the list of visited edges (arg3). 
 */
check_cycle(NodeLabel,RevVisitedNodes,RevVisitedEdges,PrevCycles,Cycles) :-
   ct_node(Id,NodeLabel),
   !,
   path_member(Id,RevVisitedNodes,RevVisitedEdges,Cycle)
     -> Cycles = [Cycle|PrevCycles]
      ; Cycles = PrevCycles.

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
  
path_member_(Id,[Id     ],[EdgeId  ],[EdgeId  ]).    % self cycle
path_member_(Id,[Id|_   ],[EdgeId|_],[EdgeId  ]) :-  % end of cycle
   !.  
path_member_(Id,[_|Nodes],[EId|Rest ],[EId|Cycle]) :-
   path_member_(Id,Nodes,Rest,Cycle).  

   
/**
 * Mark node or edge as visited.   
 */
:- dynamic(visited/1).

mark(Id) :- 
	assert( visited(Id) ).

sweep :-
    retractall(visited(_)).
    

% Unused:     
    
:- dynamic(current_cycle_analysis/1);

current_cycle_analysis(0).

next_cycle_analysis(Y) :- 
    retract(current_cycle_analysis(X)),
    retractall(current_cycle_analysis(_)), % make sure there are no others!
    Y is X+1,
    assert(current_cycle_analysis(Y)).
    