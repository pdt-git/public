:- module(parse_util, [	assert_new_node/4, 
						cleanup_nodes/0, 
						cleanup_nodes/1, 
						cleanup_computed_facts/0,
						fileT/3,
						predicateT/5, onloadT/3,
						directiveT/3, clauseT/5, literalT/6, metaT/6, headT/6,
						operatorT/8,
						dynamicT/2, transparentT/2, multifileT/2, meta_predT/2,
						termT/2, slT/3, 
						call_edge/2, pred_edge/2, onload_edge/2, load_edge/4,
						call_built_in/4,
						fileT_ri/2, predicateT_ri/4, literalT_ri/4,  
						import_dir/2, export_dir/2, load_dir/3, property_dir/3, library_dir/3,
						pos_and_vars/3, 
						error/3, warning/3]).

:- reexport('util/ctc_admin.pl').

:- dynamic fileT/3.			%fileT(Id,FileName,Module)

:- dynamic onloadT/3.		%onloadT(Id,FileId,Module)	
:- dynamic predicateT/5.	%predicateT(Id,FileId,Functor,Arity,Module)

:- dynamic directiveT/3.	%directiveT(Id,FileId,Module)
:- dynamic clauseT/5.		%clauseT(Id,ParentId,Module,Functor,Arity)
:- dynamic literalT/6.		%literalT(Id,ParentId,EnclosingId,Module,Functor,Arity)
:- dynamic metaT/6.			%metaT(Id,ParentId,EnclosingId,Module,Functor,Arity)		<-- da soll wahrscheinlich noch mehr rein...
:- dynamic headT/6.			%headT(Id,ParentId,EnclosingId,Module,Functor,Arity)

:- dynamic operatorT/8.		%operatorT(Id,ParentId,FileId,Module,Name,Arity,Type,Precedence)

:- dynamic dynamicT/2.		%dynamicT(PredicateId,DynamicId)  			
:- dynamic transparentT/2.	%transparentT(PredicateId,DynamicId)		
:- dynamic multifileT/2.	%multifileT(PredicateId,DynamicId) 
:- dynamic meta_predT/2.	%meta_predT(PredicateId,DynamicId)		

:- dynamic termT/2.			%termT(Id,Term)
:- dynamic slT/3.			%slT(Id,Pos,Len)    <-- should be coordinated with JTransformer in the long run!!!!

:- dynamic call_edge/2.		%call_edge(PredId,LiteralId)
:- dynamic pred_edge/2.		%pred_edge(ClauseId,PredId)					
:- dynamic onload_edge/2.	%onload_edge(Id,OId)						
:- dynamic load_edge/4.		%load_edge(LoadingId,FileId,Imports,Directive)

:- dynamic call_built_in/4.	%call_built_in(Functor, Arity, Module, LiteralId)

:- dynamic fileT_ri/2.		%fileT_ri(FileName,Id)
:- dynamic predicateT_ri/4.	%predicateT_ri(Functor,Arity,Module,Id)		
:- dynamic literalT_ri/4.   %literalT_ri(Functor, Arity, Module, LiteralId)

:- dynamic pos_and_vars/3.	%pos_and_vars(ClauseId,BodyPos,VarNames)

:- dynamic import_dir/2.	%import_dir(FileId,DirectiveId)
:- dynamic export_dir/2.	%export_dir(Predicates,DirectiveId)
:- dynamic load_dir/3.		%load_dir(DirectiveId,Args,Imports)
:- dynamic property_dir/3.	%property_dir(Functor,Args,DirectiveId)
:- dynamic library_dir/3.	%library_dir(LibName,LibDir,DirectiveId)

:- dynamic error/3.			%error(Error,Context,FileId)  
:- dynamic warning/3.		%warning(TreeElement,Type,AdditionalArgument)
		%TODO: warnings und errors mehr spezialisieren, so dass man sie spezieller updaten kann
		%		je nach Arbeitsschritt
		%		error aktuell nur zu Files (auf dauer wohl besser spezieller
		
		
		
/**
 * cleanup_nodes/0 isdet
 * retracts everything a former run of plparser_quick:generate_facts/1 could have asserted.
 **/  
cleanup_nodes:-
	retractall(fileT(_,_,_)),
	retractall(literalT(_,_,_,_,_,_)),
	retractall(metaT(_,_,_,_,_,_)),
	retractall(headT(_,_,_,_,_,_)),
	retractall(clauseT(_,_,_,_,_)),
	retractall(directiveT(_,_,_)),
	retractall(operatorT(_,_,_,_,_,_,_,_)),
	retractall(dynamicT(_,_)),
	retractall(load_dir(_,_,_)),
	retractall(import_dir(_,_)),
	retractall(export_dir(_,_)),
	retractall(transparentT(_,_)),						
	retractall(multifileT(_,_)),	
	retractall(meta_predT(_,_)),
	retractall(termT(_,_)),
	retractall(slT(_,_,_)),
	retractall(literalT_ri(_,_,_,_)),     
	retractall(fileT_ri(_,_)),
	retractall(pos_and_vars(_,_,_)),	%should not be there, because they get removed when bodies are parsed
	retractall(error(_,_,_)),
	retractall(warning(_,_,_)),
	cleanup_computed_facts,
	ctc_id_init.
	
cleanup_computed_facts:-
    retractall(call_edge(_,_)),	
	retractall(pred_edge(_,_)),
	retractall(onload_edge(_,_)),
	retractall(load_edge(_,_,_,_)),
	retractall(call_built_in(_,_,_,_)),
    retractall(predicateT(_,_,_,_,_)),
	retractall(onloadT(_,_,_)),
    retractall(predicateT_ri(_,_,_,_)),
	retractall(property_dir(_,_,_)),
	retractall(library_dir(_,_,_)).
    

/**
 * assert_new_node(+Term,+From,+To,-Id)
 * 	creates new identity Arg4 and asserts termT and slT with the information given
 *  by Arg1-Arg3 to this identity. 
 *  the Arg6. 
 */   
assert_new_node(Term,From,To,Id):- 
    new_node_id(Id),	
	assert(termT(Id,Term)),
    Length is To - From,
    assert(slT(Id,From,Length)).

    

cleanup_nodes(File):-
    fileT_ri(File,Id), !,
    clean_file_entries(Id),
    retractall(fileT_ri(File,Id)),
    retractall(fileT(Id,_,_)),
    retractall(error(_,_,Id)).		%TODO: gegebenenfalls zu clean_general_references_to/1 schieben
cleanup_nodes(_).
    
clean_file_entries(FileId):-
	directiveT(DirId,FileId,_),
		clean_directives(DirId),
		retractall(directiveT(DirId,_,_)),
		retractall(import_dir(_,DirId)),
		retractall(export_dir(_,DirId)),
		retractall(load_dir(DirId,_,_)),
		clean_general_references_to(DirId),
	fail.
clean_file_entries(FileId):-
	clauseT(ClauseId,FileId,_,_,_),
		clean_clause_references(ClauseId),
		retractall(clauseT(ClauseId,_,_,_,_)),
		retractall(pos_and_vars(ClauseId,_,_)),
		clean_general_references_to(ClauseId),
	fail.	
clean_file_entries(_).
	
clean_clause_references(ClauseId):-
    headT(HeadId,_,ClauseId,_,_,_),
    	clean_clause_references(HeadId),
    	retractall(headT(HeadId,_,_,_,_,_)),
    fail.
clean_clause_references(ClauseId):-   
	literalT(LitId,_,ClauseId,_,_,_),
		clean_clause_references(LitId),
    	retractall(literalT(LitId,_,ClauseId,M,F,A)),
    	retractall(literalT_ri(F,A,M,LitId)),
    	retractall(metaT(LitId,_,ClauseId,_,_,_)),
    fail.



%:- dynamic warning/3.		%warning(TreeElement,Type,AdditionalArgument)

clean_directives(DirectiveId):-
    retractall(import_dir(_,DirectiveId)),
    retractall(export_dir(_,DirectiveId)),
    retractall(load_dir(DirectiveId,_,_)),
    retractall(property_dir(_,_,DirectiveId)),
    retractall(library_dir(_,_,DirectiveId)).



clean_general_references_to(Id):-
	retractall(termT(Id,_)),
	retractall(slT(Id,_,_)),
	retractall(warning(Id,_,_)).
	