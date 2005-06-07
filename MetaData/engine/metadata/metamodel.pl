/* The predicates in this modul describe the meta structure of the model.
 * They do not describe actual nodes, but define which properties a node of
 *	a certain type may have and how nodes of given type might be connected
 * with each other.
 *
 * Developer note: all predicates within this module should be lightning fast, they
 * will propably be queried a lot. When implementing new nodes, we should make
 *	sure that their type can easily be found out from looking at their id.
 */
:-module(metamodel,
[
	meta_node/1,
	meta_node_property/2,
	meta_edge/2,
	meta_edge_property/3
]).

%% meta_node/1 defines which node types are valid.
%% the argument type should be the same atom as is used in the type(<type>) 
%% property.
meta_node(module).
meta_node(predicate).
meta_node(clause).
meta_node(source_folder).
meta_node(compilation_unit).
meta_node(atom).
meta_node(variable).
meta_node(string).
meta_node(brace).
meta_node(list).
meta_node(compound).

%% meta_node_property/2 defines properties of meta_nodes (NOT NODES!!!)
%%
%% leaf - nodes of a type with the leaf property CAN NOT have outgoing edges.
%% tree(Tree)	-	indicates that nodes of this type belong to a tree named Tree
%% editable - nodes of a tyüe with this property can be edited.
%%				currently the only means of editing implemented is 
%%				deletion of a node
%%
%% has(Property,Kind,Mult) - defines a property that a node of this type has
%%		Property - 	the name of the property. This name is used as the 
%%						property term functor in node_property/2
%%		Kind		-	one of 'flag', 'simple' or 'ordered'
%%						 -	'flag' properties have an implied multiplicity of 0-1.
%%							'flag' properties terms in node_property/2 are simple atoms.
%%						 -	'simple' properties may have any multiplicity.
%%							'simple' property terms in node_property/2 are unary terms
%%							of the form <name>(<value>).
%%						 -	'ordered' properties may have any multiplicity
%%							'ordered' property terms in node_property/2 are unary terms
%%							of the form <name>(<value>), where value is a list.
%%							Backtracking over a 'list' property should always produce
%%							exactly one list. The mutliplicity given in Mult gives
%%							informmation about the size of that list, NOT the number
%%							of lists.
%%		Mult		-	a term of the form Min-Max indicating a multiplicity, 
%%						Min and Max should be integers (including 0) or the atom 'n',
meta_node_property(variable,leaf).
meta_node_property(string,leaf).
meta_node_property(brace,leaf).
meta_node_property(module,tree(runtime)).
meta_node_property(predicate,tree(runtime)).
meta_node_property(clause,tree(runtime)).
meta_node_property(source_folder,tree(source)).
meta_node_property(compilation_unit,tree(source)).
meta_node_property(atom,tree(source)).
meta_node_property(variable,tree(source)).
meta_node_property(string,tree(source)).
meta_node_property(brace,tree(source)).
meta_node_property(list,tree(source)).
meta_node_property(compound,tree(source)).
meta_node_property(atom,tree(term)).
meta_node_property(variable,tree(term)).
meta_node_property(string,tree(term)).
meta_node_property(brace,tree(term)).
meta_node_property(list,tree(term)).
meta_node_property(compound,tree(term)).
meta_node_property(module,has(file,simple,0-n)).
meta_node_property(module,has(predicate,simple,0-n)).
meta_node_property(predicate,has(name,simple,1-1)).
meta_node_property(predicate,has(arity,simple,1-1)).
meta_node_property(predicate,has(term,simple,1-1)).
%%TODO: this should actualy be an orderd property.
meta_node_property(predicate,has(clause,simple,0-n)).
meta_node_property(predicate,has(built_in,flag,0-1)).
meta_node_property(predicate,has(dynamic,flag,0-1)).
meta_node_property(predicate,has(exported,flag,0-1)).
meta_node_property(predicate,has(imported_from,simple,0-1)).
meta_node_property(predicate,has(file,simle,0-1)).
meta_node_property(predicate,has(foreign,flag,0-1)).
meta_node_property(predicate,has(indexed,simple,0-1)).
meta_node_property(predicate,has(interpreted,flag,0-1)).
meta_node_property(predicate,has(line_count,simple,0-1)).
meta_node_property(predicate,has(multifile,flag,0-1)).
meta_node_property(predicate,has(nodebug,flag,0-1)).
meta_node_property(predicate,has(notrace,flag,0-1)).
meta_node_property(predicate,has(number_of_clauses,simple,0-1)).
meta_node_property(predicate,has(thread_local,flag,0-1)).
meta_node_property(predicate,has(transparent,flag,0-1)).
meta_node_property(predicate,has(undefined,flag,0-1)).
meta_node_property(predicate,has(volatile,flag,0-1)).
meta_node_property(clause,has(clause_ref,simple,1-1)).
meta_node_property(clause,has(clause_number,simple,1-1)).
meta_node_property(clause,has(head_term,simple,1-1)).
meta_node_property(clause,has(body_term,simple,1-1)).
meta_node_property(clause,has(file,simple,0-1)).
meta_node_property(clause,has(line_count,simple,0-1)).
meta_node_property(clause,has(fact,flag,0-1)).
meta_node_property(clause,has(erased,flag,0-1)).
meta_node_property(source_folder,has(compilation_unit,simple,0-n)).
meta_node_property(compilation_unit,has(clause,simple,0-n)).
meta_node_property(brace,has(argument,simple,0-1)).
meta_node_property(list,has(elements,ordered,0-n)).
meta_node_property(list,has(tail,simple,0-1)).
meta_node_property(compound,has(arguments,ordered,0-n)).
meta_node_property(compound,has(functor,simple,1-1)).
meta_node_property(compound,has(functor_position,simple,1-1)).


%% properties common to all term types
meta_node_property(Type,has(clause,flag,0-1)):-
    meta_node_property(Type,tree(term)).
meta_node_property(Type,has(position,simple,1-1)):-
    meta_node_property(Type,tree(term)).
meta_node_property(Type,has(line,simple,1-1)):-
    meta_node_property(Type,tree(term)).
meta_node_property(Type,has(term,simple,1-1)):-
    meta_node_property(Type,tree(term)).

%% properties common to all source types
meta_node_property(Type,has(file,simple,1-1)):-
    meta_node_property(Type,tree(source)).
meta_node_property(Type,editable):-
    meta_node_property(Type,tree(source)).

%% properties common to all node types
meta_node_property(Type,has(parent,simple,0-1)):-
    meta_node(Type).
meta_node_property(Type,has(type,simple,1-1)):-
    meta_node(Type).



%% meta_edge/1 defines what kinds of outgoing edges exist for
%% a node of a certain type.
%% Edges between nodes are properties of one node containing 
%% another node as their value.
%% Meta edges otoh describe which _possible_ edges a node of a 
%% given type might be connected to. It is important to understand
%% that they do NOT represent connections between actual nodes.
%% Meta Edges are identified by the type of the "from" node and
%% the name of a property, which embodies the edge.
%%
meta_edge(module,predicate).
meta_edge(predicate,clause).
meta_edge(source_folder,compilation_unit).
meta_edge(compilation_unit,clause).
meta_edge(brace,argument).
meta_edge(list,elements).
meta_edge(list,tail).
meta_edge(compound,arguments).
%% each node may have an edge to its parent
meta_edge(Type,parent):-
    meta_node(Type).

%% meta_edge_property/3 defines properties of meta edges.
%% Meta edges also may have properties just as meta nodes have.
%% currently the following few are defined.
%%
%%  direction(Dir)	-	either incoming or outgoing with obvisous semantics.
%%							Note: incoming edges should be "weak", that is
%%							their multiplicity constraints should always allow
%%							to remove peers.
%%  peer_property(Name)		-	Name is the name of a property
%%										by which edges of this kind can be
%%										navigated from a connected peer 
%%										in the oposite direction.
%%										For an corresponding pair of incoming 
%%										and outgoing meta edges, it is sufficient to
%%										specify this property for only one direction.
%%	tree(Tree)		-	A flag indicating this edge type induces a tree
%%							structure. This implicates that
%%								(1) 	the multiplicity of this property is 0-1
%%								(2) 	this type has no other property with the 
%%										same tree(Tree)
%%								(3)	a path on which all edges share the same
%%										tree(Tree) and direction(Direction) term cannot 
%%										be a cycle.
meta_edge_property(module,predicate,direction(outgoing)).
meta_edge_property(module,predicate,peer_property(parent)).
meta_edge_property(module,predicate,tree(runtime)).
meta_edge_property(predicate,clause,direction(outgoing)).
meta_edge_property(predicate,clause,peer_property(parent)).
meta_edge_property(predicate,clause,tree(runtime)).
meta_edge_property(source_folder,compilation_unit,direction(outgoing)).
meta_edge_property(source_folder,compilation_unit,peer_property(parent)).
meta_edge_property(source_folder,compilation_unit,tree(source)).
meta_edge_property(compilation_unit,clause,direction(outgoing)).
meta_edge_property(compilation_unit,clause,peer_property(parent)).
meta_edge_property(compilation_unit,clause,tree(source)).
meta_edge_property(brace,argument,direction(outgoing)).
meta_edge_property(brace,argument,peer_property(parent)).
meta_edge_property(brace,argument,tree(source)).
meta_edge_property(list,elements,direction(outgoing)).
meta_edge_property(list,elements,peer_property(parent)).
meta_edge_property(list,elements,tree(source)).
meta_edge_property(list,tail,direction(outgoing)).
meta_edge_property(list,tail,peer_property(parent)).
meta_edge_property(list,tail,tree(source)).
meta_edge_property(compound,arguments,direction(outgoing)).
meta_edge_property(compound,arguments,peer_property(parent)).
meta_edge_property(compound,arguments,tree(source)).
meta_edge_property(Type,parent,direction(incoming)):-
    meta_node(Type).
meta_edge_property(Type,parent,tree(source)):-
    meta_node_property(Type,tree(source)).
meta_edge_property(Type,parent,tree(runtime)):-
    meta_node_property(Type,tree(runtime)).
    