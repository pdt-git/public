:-module(blub,[]).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).

:- pdt_define_context(post(contains,warm,hot)).
:- pdt_define_context(asubst(post_map)).

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

do_goal(In,Goal,Delta):-
	mark_hot(In,Goal,In1),
    resolve_predicate(In1,Goal,Predicate),
    (	known(Predicate,Goal,In1,Delta)
    ->	true
    ;	findall(Delta1,
			(
				predicate_clause(Predicate,Clause),
				do_clause(In1,Clause,Delta1)
			),
			Deltas
		),
		lub(Deltas,Delta),
		remember(Predicate,Goal,In1,Delta)
	).


	
do_clause(In,Goal,Clause,Delta):-
	clause_head(Clause,Head),
	do_unify(In,Head,Goal,Delta1),
	lub([In,Delta1],In1),
    clause_body(Clause,Body),
    do_goal(In1,Body,Delta2),
    lub([Delta1,Delta2],Delta).

known(builtin(system,true,0),_,In,In).
known(builtin(system,':',2),Goal,In,Out):-
    match(Goal,(_A:B)),
    do_goal(In,B,Out).
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
resolve_predicate(_In,Goal,Predicate):-
    source_term_functor(Goal,Name,Arity),
    %term_context(In,Goal,Module),
    pdt_find_predicate(testmodule:Name/Arity,Predicate).
    
	    
predicate_clause(Predicate,Clause):-
    pdt_property(Predicate,clauses,Clauses),
    member(Clause,Clauses).
    

    
do_unify(In,A,B,Delta):-
    source_term_unifiable(A,B,Unifier),
    process_unifier(Unifier,In,Delta).


process_unifier([],Post,Post).
process_unifier([Var=Value|Unifier],In,Delta):-
    process_binding(In,Var,Value,Delta1),
    lub([In,Delta1],In1),
    process_unifier(Unifier,In1,Delta2),
    lub([Delta1,Delta2],Delta).

process_binding(In,Var,Value,Delta):-
	var_node(Var,VarNode),
	term_node(Value,ValueNode),
	connect(In,VarNode,ValueNode,Delta).
	

var_node(Var,VarNode):-
    get_attr(Var,pdt_annotator,VarNode).
	
term_node(Var,Node):-
	source_term_var(Var),
	!,
	source_term_property(Var,variable_id,Node).    
term_node(Term,term_id(F,N)):-
	source_term_property(Term,file_ref,F),
	source_term_property(Term,n,N).
    
    
connect(In,A,B,Delta):-
	get_post(In,A,PostA),
	get_post(In,B,PostB),
	findall(C,connection(A,B,PostB,C),ABs),
	findall(C,connection(B,A,PostA,C),BAs),
	generate_post(ABs,PostADelta),
	generate_post(BAs,PostBDelta),
	asubst_new(Delta0),	
	pdt_map_empty(PostMap),
	asubst_post_map(Delta0,PostMap),
	set_post(Delta0,A,PostADelta,Delta1),
	set_post(Delta1,B,PostBDelta,Delta).


get_post(In,Node,Post):-
    asubst_post_map(In,Map),
    pdt_map_get(Map,Node,Post),
    !.
get_post(_,_,Post):-
    generate_post([],Post).

generate_post(ABs,Post):-
    post_new(Post0),
    pdt_set_empty(Contains),
    pdt_set_empty(Warm),
    pdt_set_empty(Hot),
    post_contains(Post0,Contains),
    post_warm(Post0,Warm),
    post_hot(Post0,Hot),
    add_connections(ABs,Post0,Post).

    
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
    
lub([],Out):-
	asubst_new(Out),
	pdt_map_empty(Map),
	asubst_post_map(Map).
lub([A],A):-
	!.	
lub([A|As],Out):-
    merge_all(As,A,Out).

merge_all([],In,In).
merge_all([A|As],In,Out):-
    merge(In,A,Next),
    merge_all(Next,As,Out).


merge(In,Add,Out):-
    asubst_post_map(In,Map0),
    asubst_post_map(Add,AddMap),
	pdt_map_putall(Map0,Node,Post,
		(	pdt_map_get(AddMap,Node,AddPost),
			merge_post(Map0,Node,AddPost,Post)
		),
		Map
	),
	asubst_set_post_map(In,Map,Out).
    
merge_post(Map0,Node,AddPost,Post):-
	pdt_map_get(Map0,Node,Post0),
	!,
	merge_args(Post0,AddPost,Post).
merge_post(_Map0,_Node,AddPost,AddPost).

merge_args(Post0,AddPost,Post):-
    post_new(Post),
    functor(Post,_,Arity),
    merge_args(1,Post0,AddPost,Arity,Post).

merge_args(N,_Post0,_AddPost,Arity,_Post):-
    N>Arity,
    !.
merge_args(N,Post0,AddPost,Arity,Post):-   
	arg(N,Post0,Set0),
	arg(N,AddPost,Set),  
	merge_sets(Set0,Set,NewSet),
	arg(N,Post,NewSet),
    M is N+1,
    merge_args(M,Post0,AddPost,Arity,Post).


merge_sets(Set0,Set,Set):-
    pdt_set_empty(Set0),
    !.
merge_sets(Set0,Set,NewSet):-    
	pdt_set_addall(Set0,Elm,pdt_set_element(Set,Elm),NewSet).

%TODO    
remember(Predicate,Goal,In,Out).
%TODO    
known(Predicate,Goal,In1,Delta):-
    fail.
%TODO
mark_context(In,B,A,In).

