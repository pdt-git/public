check_node(Check,Id,Type,Label,Props):-        
    catch(
    (
      exists(node(Id,Type,Label)),
      unique(node(Id,_,_)),
      properties(Id,Props)
    ),
    E,
    ( findall(P,property(Id,P),L),
      throw(check_failed(Check,E,L)))
    ).

check_edge(Check,Id,Type,Label,From,To):-
    catch(
    (
      exists(edge(Id,Type,Label,From,To)),
      unique(edge(Id,_,_,_,_))
    ),
    E,
    throw(check_failed(Check,E))).
properties(_,[]).
properties(Id,[Prop|Tail]):-  
	exists(property(Id,Prop)),  
    unique(property(Id,Prop)),
    properties(Id,Tail).

exists(Goal):-
  Goal;throw(exists_failed(Goal)).
  
unique(Goal):-
  findall(Goal,Goal,L),
  L=[Goal];throw(unique_failed(Goal,L)).
        