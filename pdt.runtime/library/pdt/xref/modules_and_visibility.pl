:- module(modules_and_visibility, [	compute_visibility_graph/0]).

:- use_module(parse_util).

:- dynamic exporting/3.	%exporting(Module,PredId,Directive)

compute_visibility_graph:-
    compute_exports.
    
    
compute_exports:-
    retractall(exporting(_,_,_)),
    export_dir(Exports,Directive),
%    plparser_quick:export_dir(Exports,Directive),
    	flatten(Exports,ExportsFlatt),
%    	format('~w ~n',[ExportsFlatt]),
    	build_export_edge_from_list(ExportsFlatt,Directive),
    	fail.
compute_exports.
    
build_export_edge_from_list([],_,_).    
build_export_edge_from_list([A|B],Directive):-
    build_export_edge(A,Directive),
    build_export_edge_from_list(B,Directive).
    
build_export_edge(Functor/Arity,Directive):-
    directiveT(Directive,_,Module),
    plparser_quick:predicateT_ri(Functor,Arity,Module,Id),      
    assert(exporting(Module,Id,Directive)),
    !.
    

visible_in_module(Predicate,Module):-
    visible_in_module_as(Predicate,Module,_).
    
visible_in_module_as(Predicate,Module,Functor):-
    predicateT(Predicate,_,Functor,_,Module). %,
%    !.
visible_in_module_as(Predicate,Module,Functor):-
    not( predicateT(Predicate,_,Functor,_,Module)),
    fileT(ModuleFile,_,Module),
    load_edge(ModuleFile,DefiningFile,Imports,_),
    fileT(DefiningFile,_,DefiningModule),
    visible_in_module_as(Predicate,DefiningModule,DefiningFunctor),
    exporting(DefiningModule,Predicate,_),
    compute_importing_functor(Imports,DefiningFunctor,Functor).     %Eva: !!!!! TEST this!!!!!    
   
compute_importing_functor(all,Functor,Functor):-
    !.
compute_importing_functor([A|B],Functor,NewFunctor):-
	compute_importing_functor_for_list([A|B],Functor,NewFunctor).
compute_importing_functor(except(List),Functor,NewFunctor):-   
    compute_importing_functor_with_exceptions(List,Functor,NewFunctor).
   
   
compute_importing_functor_for_list([A|B],Functor,NewFunctor):-
    (	(is_searched_functor(A,Functor,NewFunctor), !)
    ;	compute_importing_functor_for_list(B,Functor,NewFunctor)
    ).
    
compute_importing_functor_with_exceptions([],Functor,Functor).
compute_importing_functor_with_exceptions([A|B],Functor,NewFunctor):-
    (	is_searched_functor(A,Functor,AFunctor)
    ->	(	Functor == AFunctor
    	->	fail
    	;	NewFunctor = AFunctor
    	)
    ;	compute_importing_functor_for_list(B,Functor,NewFunctor)
    ).
    
    
    
is_searched_functor(Functor/_Arity,Functor,Functor):-
    !.
is_searched_functor(Functor/_A 'as' NewFunctor,Functor,NewFunctor):- 
    !.   
     



    
