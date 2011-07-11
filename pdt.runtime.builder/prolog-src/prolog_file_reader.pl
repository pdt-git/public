:- module(plparser_quick,[	generate_facts/1]).

:- consult('util/walking_prolog_files.pl').
:- reexport(parse_util).
:- use_module(preparser).
:- use_module(predicates).
:- use_module(load_graph).
:- use_module(modules_and_visibility).
:- use_module(literal_parser).
:- use_module(org_cs3_lp_utils(utils4modules)).


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
	writeln('compute_visibilities'),
	time(compute_visibility_graph),
	writeln('parse literals'),
	time(parse_bodies),
	writeln('generate edges'),
	time(derive_edges).    
	
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
	
update_facts(File, Project):-				
	cleanup_nodes(File),
	cleanup_computed_facts,
   	walking_file_list(Project,parse,1),	
	build_load_graph,
    derive_all_predicates,
	derive_onloads,
	compute_all_predicate_properties,
	compute_visibility_graph,
	parse_bodies,
	derive_edges.
        
derive_edges:-
    forall( 
    	(	literalT(LId,_,CId,Module,Functor,Arity)
    	;	metaT(LId,_,CId,Module,Functor,Arity)
    	),
    	( 	(	(	predicateT_ri(Functor,Arity,Module,Id)
    			->	true
    			;	(	predicateT_ri(Functor,Arity,_DefModule,Id),
    					visible_in_module(Id, Module)
%    					(	visible_in_module(Id, Module)
%    					->	true
%    					;	(	clauseT(CId,ParentId,_Module,_Functor,_Arity),
%    							fileT(ParentId, File, _),	
%    							format('visibility of ~w:~w/~w for Module ~w not found; calling File: ~w~n', [_DefModule, Functor, Arity, Module, File])
%    						)
%    					)
   					)
   				)
   			->	assert(call_edge(Id,LId))	
			;	(	functor(Term, Functor, Arity),
    		  		(	(	%format('Call to ~w:~w/~w~n', [Module, Functor, Arity]),
    		  				declared_in_module(Module, Term, DefModule),
    		  				%format('Defined in ~w~n', [DefModule]),
    		  				predicate_property(DefModule:Term, built_in)
    		  			)
    		  		->	assert(call_built_in(Functor, Arity, DefModule, LId))
    		  		;	%format('Call to ~w ~w:~w/~w -> Not found.~n', [Module, DefModule, Functor, Arity])
    		  			true			%TODO: here is a possible place to create a warning as soon as it's reduced to "real" problems... 
    		  		)
    		  	)
			)
    	)
    ).
