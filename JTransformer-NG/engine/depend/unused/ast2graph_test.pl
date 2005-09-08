test(ast2graph1):-
	findall([Name,Edge], 
	  ast_edge(Name,methodDefT(_Id,Parent,_MName,_Params,_Type,_Exs,Body),Edge),
	  List),
	assert_true('method has parent and a body link, if params are not bound',
	   List = [[parent,Parent],[body,Body]]).
    
test(ast2graph2):-
	findall([Name,Edge], 
	  ast_edge(Name,methodDefT(_Id,Parent,_MName,[P1,P2],_Type,_Exs,Body),Edge),
	  List),
	assert_true('method has parent and a body link, if params are not bound',
	   List = [[parent,Parent],[params,P1],[params,P2],[body,Body]]).
        