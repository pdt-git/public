:- module(parse_util_old,[	generate_facts/1, 
							update_facts/2]).

:- ensure_loaded('util/walking_prolog_files.pl').
:- reexport(pdt_factbase).
:- use_module(preparser).
:- use_module(predicates).
:- use_module(load_graph).
:- use_module(modules_and_visibility).
:- use_module(literal_parser).
:- use_module(cross_reference_builder).

generate_facts(Project):-
    cleanup_nodes,
    walking_file_list(Project,parse,1),
	build_load_graph,
    derive_all_predicates,
	derive_onloads,
	compute_all_predicate_properties,
	compute_visibility_graph,
	parse_bodies,
	derive_edges.    

%generate_facts(Project):-
%    writeln('cleaning up'),
%    cleanup_nodes,
%    writeln('start parsing clauses'),
%	time(walking_file_list(Project,parse,1)),
%	writeln('generating loadgraph'),
%	time(build_load_graph),
%    writeln('generating predicates'),
%	time(derive_all_predicates),
%	writeln('genereating directive collections'),
%	time(derive_onloads),
%	writeln('compute_predicate_properties'),
%	time(compute_all_predicate_properties),
%	writeln('compute_visibilities'),
%	time(compute_visibility_graph),
%	writeln('parse literals'),
%	time(parse_bodies),
%	writeln('generate edges'),
%	time(derive_edges).    
	
update_facts(File, Project):-				
	cleanup_nodes(File),
	cleanup_computed_facts,
   	walking_file_list([File|Project],parse,1),	
	build_load_graph,
    derive_all_predicates,
	derive_onloads,
	compute_all_predicate_properties,
	compute_visibility_graph,
	parse_bodies,
	derive_edges.
	
%update_facts(File, Project):-				
%	format('cleaning up facts for ~w~n',File),
%	cleanup_nodes(File),
%	cleanup_computed_facts,
%    writeln('start parsing clauses'),			
%	time(walking_file_list(Project,parse,1)),	
%	writeln('generating loadgraph'),
%	time(build_load_graph),
%    writeln('generating predicates'),
%	time(derive_all_predicates),
%	writeln('genereating directive collections'),
%	time(derive_onloads),
%	writeln('compute_predicate_properties'),
%	time(compute_all_predicate_properties),
%	writeln('compute_visibilities'),
%	time(compute_visibility_graph),
%	writeln('parse literals'),
%	time(parse_bodies),
%	writeln('generate edges'),
%	time(derive_edges).
	
