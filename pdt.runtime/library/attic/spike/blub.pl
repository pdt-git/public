:-module(blub,[]).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).

:- pdt_define_context(post(contains,warm,hot)).
:- pdt_define_context(asubst(post)).

/*
expand known meta predicates
 - mark called arguments as "hot"
 - if argument is a variable, pass on the "heat" to other occurances within the clause.
 - for any subterm that changes from "cold" to "warm" or "hot", increase the temperature of the surrounding
   term to "warm" (if it is not already "warm" or "hot").
   
*/

/*
What I want to do: calculate a program "markup" (in lack of a better term).

=The Big picture=:
A) mark all called arguments of known meta predicates as "hot".
B) If a variable is marked as "hot", draw an edge from this variable to the meta call site, 
   and label it "hot".
C) If the variable is unified with another node, draw an edge from this other node to the 
   variable, label it "hot"
D) If the variable apears (within the same "and scope") in another term, add an edge
   to from this term to the variable and label it "contains". The term is concidered as 
   "warm" now.
E) If a "warm" term is unified with another term, draw an edge from that other term to
   the "warm" term, label it warm.

In the end, it should be possible to find out which parts of a term are executed by 
simple unification along the edges created by the graph.

markup process is similar to (but not the same as) the abstract interpretation scheme.
Abstract domain in our case is the set of all possible markups of the programm.
The size of the abstract domain therefor is bounded by 2^(|P|^2). 
To estimate the costs for finding the "least fix point" (not sure if this term is still valid 
in my setup) there is probably a much tighter bound, but i haven't yet found the time to 
investigate this.

=Inverse order of execution=
I inverse the order of execution within each clause. This way i don't have to carry around 
useless unification edges - remember i am only interested in paths leading to call sites.

=LUB=
The LUB (at OR-nodes) is really just the set-union of all edges found by the children. 
Funny, but it really seems to "just" work out. Have to do some proving to erase my last remaining 
doubts on this.
Doing a set union every time would be rather costy. So, instead of letting each
computation step provide a modified copy of the original abstract substitution, i will
instead let each clause start a new set from scratch. After processing an OR node is
done, these "deltas" will be merged into the original substitution, which should be
significantly cheaper.


=Fixpoint Iteration=
I am trying to formulate this based on update events. I currently think that
there are two kinds of events that trigger an update

A) A predicate's "meta-predicate-ness" changes. This is a case whenever
a new outgoing edge is added to a non-ground argument of a clause head.
All references to this predicate that are already known have to be revisited
Afaics, references can only exist if there is a recursion within the predicate. Otherwise, 
the "meta-predicate-ness" would have been detected before.

B) A predicate is called with a substitution that was not covered by previous passes.
Of course I do not want to redo every predicate definition each time I encounter a call of that
predicate. So I only check if outgoing edges were added to the predicate arguments (you can see 
this as an abstraction of all markups that have those edges).
A further level of indirection is introduced between a (logical) predicate definition and
the set of contributing clauses. If an outgoing edge is added to the argument of a predicate 
call site which changes the temperature of the argument, an edge has to be added pointing from
the argument of the predicate definition to the argument of the call site. Further edges have to
be added pointing from the corresponding argument in the clause heads towards the argument of the
predicate definition. Now, instead of revisiting the whole predicate definition and all its 
clauses whenever an edge is added to the predicate definition arguments, I only revisit the 
clauses whenever an edge is added to the clause head arguments. 
This can be viewed as postponing a huge part of the actual fixpoint calculation until a later 
stage - the markup created by this algorithm can be seen as an intermidiate result. 
For now, I just transfer the problem to a graph problem.
To find out, for instance, which terms in the programm are call sites, you basically have to 
calculate a transitive hull of that graph. (This is not the whole story, but it is the 
main part of it)

=Module-transparent predicates=
... must be treated separately for each context module they are called from.
For my use it is feasable to regard them as templates that are instantiated in each 
context they are used in. A simple way to represent this is by parameterising EVERY 
predicate by its respective context module. For non-transparent predicates, there will
be exactly one instance - the one corresponding to the predicates definition module.
For module-transparent predicates, there will be several instances.
*/

do_goal(Base,In,Goal,Out):-
	lub([Base,In],Base1),
	asubst_new(In1),
	pdt_map_empty(Post),
	asubst_post(In1,Post),  	
    mark_hot(In1,Goal,In2),
    resolve_predicate(In2,Goal,Predicate),
    (	known(Predicate,Goal,Base1,In2,Out)
    ->	true
    ;	findall(OutN,
			(
				predicate_clause(Predicate,Clause),
				do_clause(Base1,In2,Clause,OutN)
			),
			Outs
		),
		lub([In|Outs],Out),
		remember(Predicate,Goal,Base1,In2,Out)
	).


	
do_clause(Base,In,Goal,Clause,Out):-
	clause_head(Clause,Head),
	do_unify(Base,In,Head,Goal,Next),
    clause_body(Clause,Body),
    do_goal(Base,Next,Body,Out).

known(builtin(system,true,0),_,In,In).
known(builtin(system,':',2),Goal,In,Out):-
    match(Goal,(A:B)),
    mark_context(In,B,A,Next),
    do_goal(Next,B,Out).
known(builtin(system,',',2),Goal,In,Out):-
    match(Goal,(A,B)),
    do_goal(In,A,Next),
    do_goal(Next,B,Out).
   
clause_body(Clause,Body):-
	match(Clause,':-'(_Head,Body)),
	!.
clause_body(_Clause,true).

clause_head(Clause,Head):-
    match(Clause,':-'(Head,_Body)),
    !.
clause_head(Clause,Clause).

mark_hot(In,Goal,Out):-
    term_temperature(Goal,Temp),
    (	Temp==hot
    ->	In=Out
    ;	term_ref(Goal,Ref),
    	ctx_heatmap(In,Map0),
	    pdt_map_put(Map0,Ref,hot,Map1),
	    ctx_set_heatmap(In,Map1,Next),
    	(	Temp==cold
	    ->	term_parent(Goal,Parent),
	    	mark_warm(Next,Parent,Out)
    	;	Next=Out
    	)
    ).

mark_warm(In,Goal,Out):-
    term_temperature(Goal,Temp),
    (	(	Temp==hot
    	;	Temp==warm
    	)
    ->	In=Out
    ;	term_ref(Goal,Ref),
    	ctx_heatmap(In,Map0),
	    pdt_map_put(Map0,Ref,warm,Map1),
	    ctx_set_heatmap(In,Map1,Next),
    	(	Temp==cold
	    ->	term_parent(Goal,Parent),
	    	mark_warm(Next,Parent,Out)
    	;	Next=Out
    	)
    ).

    
%FIXME: very simplistic implementation for now
match(Goal,Template):-
    source_term_functor(Goal,Name,Arity),
    functor(Template,Name,Arity),
    match_args(Goal,Template,1,Arity).

match_args(_,_,N,Arity):-
    N>Arity,
    !.
match_args(Goal,Template,N,Arity):-
	source_term_arg(N,Goal,Value),
	arg(N,Template,Value),
	M is N +1,
	match_args(Goal,Template,M,Arity).    

%FIXME: this is an "aproximation" for now
% it only "resoves" predicates in the module "testmodule"
resolve_predicate(In,Goal,Predicate):-
    source_term_functor(Goal,Name,Arity),
    term_context(In,Goal,Module),
    pdt_find_predicate(testmodule:Name/Arity,Predicate).
    
	    
predicate_clause(Predicate,Clause):-
    pdt_property(Predicate,clauses,Clauses),
    member(Clause,Clauses).
    


do_unify(Base,In,A,B,Out):-
    source_term_unifiable(A,B,Unifier),
    process_unifier(Unifier,Base,In,Out).


process_unifier([],_Base,Post,Post).
process_unifier([Var=Value|Unifier],Base,In,Out):-
    process_binding(Base,In,Var,Value,Next),
    process_unifier(Unifier,Base,Next,Out).

process_binding(Base,In,Var,Value,Out):-
	var_node(Var,VarNode),
	term_node(Value,ValueNode),
	connect(Base,In,VarNode,ValueNode,Out).
	

var_node(Var,VarNode):-
    get_attr(Var,pdt_annotator,VarNode).
	
term_node(Var,Node):-
	source_term_var(Var),
	!,
	source_term_property(Var,variable_id,Node).    
term_node(Term,term_id(F,N)):-
	source_term_property(Term,file_ref,F),
	source_term_property(Term,n,N).
    
    
connect(Base,In,A,B,OutPost):-
	get_post(InPost,A,PostA),
	get_post(InPost,B,PostB),
	findall(C,connection(A,B,PostB,C),ABs),
	findall(C,connection(B,A,PostA,C),BAs),
	add_connections(ABs,PostA,PostAOut),
	add_connections(BAs,PostB,PostBOut),
	pdt_map_put(InPost,A,PostAOut,NextPost),
	pdt_map_put(NextPost,B,PostBOut,OutPost).

get_post(Map,Node,Post):-
    pdt_map_get(Map,Node,Post),
    !.
get_post(_,_,Post):-
    post_new(Post),
    pdt_set_empty(Contains),
    pdt_set_empty(Warm),
    pdt_set_empty(Hot),
    post_contains(Post,Contains),
    post_warm(Post,Warm),
    post_hot(Post,Hot).
    
connection(A,B,PostB,connection(warm,A,B)):-
    post_contains(PostB,Contains),
    \+ pdt_set_empty(Contains).
connection(A,B,PostB,connection(warm,A,B)):-
    post_warm(PostB,Warm),
    \+ pdt_set_empty(Warm).
connection(A,B,PostB,connection(hot,A,B)):-
    post_hot(PostB,Hot),
    \+ pdt_set_empty(Hot).

add_connections([],Post,Post).
add_connections([connection(Label,_From,To)|Connections],InPost,OutPost):-
    post_get(InPost,[Label=InSet]),
    pdt_set_add(InSet,To,OutSet),
    post_set(InPost,[Label=OutSet],NextPost),
    add_connections(Connections,NextPost,OutPost).
    
lub(Base,Deltas,Out):-
    
remember(Predicate,Goal,In,Out)
mark_context(In,B,A,Next),
term_ref(Goal,Ref),				
