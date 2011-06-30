:- module(plparser_quick,[	generate_facts/1]).

:- consult('util/walking_prolog_files.pl').
:- reexport(parse_util).
:- use_module(preparser).
:- use_module(predicates).
:- use_module(load_graph).
:- use_module(modules_and_visibility).
:- use_module(literal_parser).


generate_facts(Project):-
    writeln('cleaning up'),
    cleanup_nodes,
    writeln('start parsing clauses'),
	time(walking_file_list(Project,parse,1)),
	writeln('generating loadgraph'),
	time(build_load_graph),
    writeln('generating predicates'),
	time(derive_all_predicates),
	writeln('genereating directive collections'),
	time(derive_onloads),
	writeln('compute_predicate_properties'),
	time(compute_all_predicate_properties),
	writeln('parse literals'),
	time(parse_bodies),
	writeln('generate edges'),
	time(derive_edges).    
        
derive_edges:-
    forall( (	literalT(LId,_,_,_Module,Functor,Arity)
    		;	metaT(LId,_,_,_Module,Functor,Arity)
    		),
    		( (  clauseT_ri(Functor,Arity,Id), 
    			% clauseT(Id,_,Module,Functor,Arity),    %Eva: umstellen auf PredicateT?
     			 assert(call_edge(LId,Id))	
    		  )
    		  ;	true
    		)
    	  ).
/****
* Eva: Die Modul-Sichtbarkeit berücksichtigen!!!!!!!!
*/
