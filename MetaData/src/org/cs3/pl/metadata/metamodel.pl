:-module(metamodule,
[
	meta_node/1,
	meta_node_property/2,
	meta_edge/1,
	meta_edge_property/2
]).

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

meta_node_property(variable,leaf).
meta_node_property(string,leaf).
meta_node_property(brace,leaf).


%% argument syntax is <node_type>(<child_property_name>)
meta_edge(module(predicate)).
meta_edge(predicate(clause)).
meta_edge(source_folder(compilation_unit)).
meta_edge(compilation_unit(clause)).
meta_edge(brace(argument)).
meta_edge(list(elements)).
meta_edge(list(tail)).
meta_edge(compound(arguments)).