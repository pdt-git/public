:- module(model_utils,[
	node/2,
	parse_property/2,
	parse_property_list/2,
	write_node/1,
	write_tree/1,
	delete_tree/1
]).

:- use_module(model).


node(Id,List):-
    var(List),
    !,
    setof(Att,node_property(Id,Att),List).
node(Id,[]):-
    node(Id).
node(Id,[HeadAttr|TailAttr]):-
    nonvar(HeadAttr),
    nonvar(TailAttr),
    !,
    node_property(Id,HeadAttr),
   	node(Id,TailAttr).
   	       
write_node(Id):-
    format("<<~w>>~n",Id),
    forall(node_property(Id,P),format(" --> ~w~n",[P])).
   	

write_tree(Id):-
	write_tree(Id,'','').
	   	
write_tree(Id,Indent,Arrow):-
    format("~a~a<<~w>>~n",[Indent,Arrow,Id]),
    atom_concat(Indent,'|    ',ChildIndent),
    forall(post(Id,Child),write_tree(Child,ChildIndent,'|--')).    

delete_tree(Node):-
    is_source(Node),
    delete_tree_rec(Node).

delete_tree_rec(Node):-
	forall(post(Node,Child),delete_tree_rec(Child)),
	retractall(node_attr(Node,_)),
	retractall(node_id(Node)).
			
    
%TODO find a better name for these predicates.
parse_property(In,Out):-
	In=..[Functor|Args],
	parse_property(Functor,Args,Out).
parse_property(Functor,[],Functor->true).
parse_property(Functor,[Arg|[]],Functor->Arg).		    

parse_property_list(In,Out):-
    maplist(parse_property,In,Out).

post(Node,Child):-
	adjacent(Node,outgoing,_,Child).