
overlong_method_signature(Max,FQN,Name,ParamList,Len):-
   methodDefT(_, Class, Name,Param,_,_,_),
   length(Param,Len), 
   Len > Max,
   fullQualifiedName(Class, FQN),
   gen_tree_list(Param,ParamList).
	 
	 