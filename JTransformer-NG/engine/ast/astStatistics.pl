% Author: Günter Kniesel
% Date: 21.07.2005

/**
 * Statistics for the new PEF database layout.
 */
 
ast_node_count(Nr) :-
   setof(Id, new_ast_node(Id,Label), AllIds),
   length(AllIds,Nr).
   
ast_node_count_by_id(Nr) :-
   setof(Id, Label^new_ast_node(Id,Label), AllIds),
   length(AllIds,Nr).
   
ast_node_count_by_label(Nr) :-
   setof(Label, Id^new_ast_node(Id,Label), AllLabels),
   length(AllLabels,Nr).
   
   
/* ast_edge_count(Nr) :-
   setof(Id, new_ast_edge(Id,Label,Id2), AllIds),
   length(AllIds,Nr).

ast_edge_count_by_label(Nr) :-
   setof(Label, Id^new_ast_edge(Id,Label,Id2), AllLabels),
   length(AllLabels,Nr).
   
*/


ast_edge_count(Nr) :-
   succeeds_n_times( new_ast_edge(Id,Label,Id2), Nr).

ast_parent_edge_count(Nr) :-
   succeeds_n_times( new_ast_edge(Id,parent,Id2), Nr).
   
succeeds_n_times(Goal, Times) :-
   (   flag(succeeds_n_times, Old, 0),
       Goal,
          flag(succeeds_n_times, N, N+1),
       fail
   ;   flag(succeeds_n_times, Times, Times)
   ).

ast_edge_list(List) :-
   succeeds_n_lists( new_ast_edge(Id,Label,Id2), List).
   
succeeds_n_lists(Goal, List) :-
   (   flag(succeeds_n_lists, Old, []),
       Goal,
          flag(succeeds_n_lists, List, [Goal|List]),
       fail
   ;   flag(succeeds_n_lists, List, List)
   ).
   
