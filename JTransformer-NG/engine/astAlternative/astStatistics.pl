% Author: Günter Kniesel
% Date: 21.07.2005

/**
 * Statistics for the new PEF database layout.
 */
 
ast_node_count(Nr) :-
   setof(Id, new_ast_node(Id,_Label), AllIds),
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
   findall( new_ast_edge(_Id,_Label,_Id2), All),
   length(All,Nr).

ast_parent_edge_count(Nr) :-
   findall( new_ast_edge(_Id,parent,_Id2), All),
   length(All,Nr).

ast_edge_list(List) :-
   findall( new_ast_edge(_Id,_Label,_Id2), List).
  
