num_classes_extern(Num):-
	findall(C,(class(C,_,_),externT(C)),L), 
	length(L, Num). 

num_classes(Num):-
	findall(C,class(C,_,_),L), 
	length(L, Num).

num_field(Num):-
	findall(C,fieldDefT(C,_,_,_,_),L), 
	length(L, Num).
num_field_extern(Num):-
	findall(C,(fieldDefT(C,Class,_,_,_),externT(Class)),L), 
	length(L, Num).
	
num_method(Num):-
	findall(C,methodDefT(C,_,_,_,_,_,_),L), 
	length(L, Num).
num_method_extern(Num):-
	findall(C,(methodDefT(C,Class,_,_,_,_,_),externT(Class)),L), 
	length(L, Num).
	
num_method_calls(Num):-
	findall(C,(applyT(C,_,_,_,_,_,_)),L), 
	length(L, Num).

overlong_method_signature(Max,FQN,Name,ParamList,Len):-
   methodDefT(_, Class, Name,Param,_,_,_),
   length(Param,Len), 
   Len > Max,
   fullQualifiedName(Class, FQN),
   gen_tree_list(Param,ParamList).
	 