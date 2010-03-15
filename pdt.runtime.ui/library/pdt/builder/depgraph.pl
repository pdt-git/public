:- module(target_graph,[print_graph/0]).
:- use_module(builder).

pdt_builder:dump_hook(Data):-
    format(atom(Base),"~w",[Data]),
    tmp_file(Base,Tmp),
    tell(Tmp),call_cleanup(print_graph,told).

node(Id,AttrString):-
   pdt_builder:'$target_state'(Id,state(A,S,_,_,_)),
   (	S==outdated
   ->	Color=red
   ;	S==available
   ->	Color=green
   ;	Color=yellow
   ),
   pdt_builder:target_key(Label,Id),
   format(string(AttrString),"[label=\"{~w|~w|~w|~w}\",fillcolor=~w]",[Id,Label,A,S,Color]).
node(Id,AttrString):-
   pdt_builder:'$target_depends_inv'(Id,_),
   \+ pdt_builder:'$target_state'(Id,_),
   pdt_builder:target_key(Label,Id),
   format(string(AttrString),"[label=\"{~w|~w|~w|~w}\",fillcolor=~w]",[Id,Label,idle,outdated,grey]).
node(Id,AttrString):-
   pdt_builder:'$target_depends'(Id,_),
   \+ pdt_builder:'$target_state'(Id,_),
   \+ pdt_builder:'$target_depends_inv'(Id,_),
   pdt_builder:target_key(Label,Id),
   format(string(AttrString),"[label=\"{~w|~w|~w|~w}\",fillcolor=~w]",[Id,Label,idle,outdated,grey]).
node(Id,AttrString):-
    current_thread(Id,Status),
    format(string(AttrString),"[shape=ellipse,style=solid,label=\"{~w|~w}\"]",[Id,Status]).


has_lock(From,To,Client):-
    pdt_builder:current_target_state(To,state(_,_,Ls,_,_)),
    member(lock(Client,From),Ls).
    
edge(From,To,AttrString):-
	pdt_builder:'$target_depends'(From,To),
	(	has_lock(From,To,Client)
	*->	format(string(AttrString),"[label=\"~w\",style=solid, color=black]",[Client])
	;	format(string(AttrString),"[label=\"dep\",style=dashed, color=black]",[])
	).
edge(From,To,AttrString):-
    pdt_builder:'$target_state'(To,state(_,_,Ls,_,_)),
    member(lock(From,[]),Ls),
    format(string(AttrString),"[style=solid, label=\"lock\"]",[]).
edge(From,To,AttrString):-
    pdt_builder:'$thread_waits'(From,To),
    format(string(AttrString),"[style=solid, label=\"wait\", color=red]",[]).
edge(From,To,AttrString):-
    pdt_builder:'$target_state'(From,state(building(To),_,_,_,_)),    
    format(string(AttrString),"[style=solid, label=\"pending\", color=blue]",[]).
edge(From,To,AttrString):-
    pdt_builder:'$target_state'(To,state(_,_,_,_,Ws)),
    member(From,Ws), 
    format(string(AttrString),"[style=solid, label=\"waiting\", color=blue]",[]).
    
	
print_graph:-   	 
    format("digraph G {~nnode [style=filled,shape = \"record\"]~n",[]),
    forall(
    	node(Id,AttrString),
    	format("\"~w\" ~w~n",[Id,AttrString])    	
    ),    
    forall(
    	edge(From,To,AttrString),
    	format("\"~w\" -> \"~w\" ~s~n",[From,To,AttrString])    	
    ),
	format("}~n",[]).
	    