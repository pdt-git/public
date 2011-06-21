:- module(parse_util, [assert_new_node/4, cleanup_nodes/0,
						fileT/3,
						clauseT/5, directiveT/3, headT/6,
						literalT/6, metaT/6,
						predicateT/5, onloadT/3, operatorT/8,
						slT/3, termT/2,
						dynamicT/5, transparentT/5, multifileT/5,
						call_edge/2, pred_edge/2, onload_edge/2, load_edge/4,
						clauseT_ri/3, predicateT_ri/4, fileT_ri/2,
						import_dir/2, export_dir/2, load_dir/3, property_dir/5, library_dir/3,
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

:- dynamic dynamicT/5.		%dynamicT(Id,Functor,Arity,Module,ParentId)  			
:- dynamic transparentT/5.	%transparentT(Id,Functor,Arity,Module,ParentId) 		
:- dynamic multifileT/5.	%multifileT(Id,Functor,Arity,Module,ParentId) 		

:- dynamic termT/2.			%termT(Id,Term)
:- dynamic slT/3.			%slT(Id,Pos,Len)    <-- should be coordinated with JTransformer in the long run!!!!

:- dynamic call_edge/2.		%call_edge(LId,Id)
:- dynamic pred_edge/2.		%pred_edge(ClauseId,PredId)							<-- ATTENTION: otherway as call_edge
:- dynamic onload_edge/2.	%onload_edge(Id,OId)						<-- ATTENTION: otherway as call_edge
:- dynamic load_edge/4.		%load_edge(LoadingId,FileId,Imports,Directive)


:- dynamic fileT_ri/2.		%fileT_ri(FileName,Id)
:- dynamic predicateT_ri/4.	%predicateT_ri(Functor,Arity,Module,Id)		
:- dynamic clauseT_ri/3.    %clauseT_ri(Functor,Arity,ClauseId)

:- dynamic pos_and_vars/3.	%pos_and_vars(ClauseId,BodyPos,VarNames)

:- dynamic import_dir/2.	%import_dir(FileId,DirectiveId)
:- dynamic export_dir/2.	%export_dir(Predicates,DirectiveId)
:- dynamic load_dir/3.		%load_dir(DirectiveId,Args,Imports)
:- dynamic property_dir/5.	%property_dir(FileId,Functor,Args,DirectiveId,Pos)
:- dynamic library_dir/3.	%library_dir(LibName,LibDir,DirectiveId)

:- dynamic error/3.			%error(Error,Context,FileId)  
:- dynamic warning/3.		%warning(TreeElement,Type,AdditionalArgument)

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
	retractall(predicateT(_,_,_,_,_)),
	retractall(onloadT(_,_,_)),
	retractall(operatorT(_,_,_,_,_,_,_,_)),
	retractall(dynamicT(_,_,_,_,_)),
	retractall(transparentT(_,_,_,_,_)),						
	retractall(multifileT(_,_,_,_,_)),	
	retractall(termT(_,_)),
	retractall(slT(_,_,_)),
	retractall(call_edge(_,_)),	
	retractall(pred_edge(_,_)),
	retractall(onload_edge(_,_)),
	retractall(load_edge(_,_,_,_)),
	retractall(clauseT_ri(_,_,_)),     
	retractall(predicateT_ri(_,_,_,_)),
	retractall(fileT_ri(_,_)),
	retractall(pos_and_vars(_,_,_)),
	retractall(import_dir(_,_)),
	retractall(export_dir(_,_)),
	retractall(load_dir(_,_,_)),
	retractall(property_dir(_,_,_,_,_)),
	retractall(library_dir(_,_,_)),
	retractall(error(_,_,_)),
	retractall(warning(_,_,_)),
	ctc_id_init.

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
