/**
  * Zugriff aus Sicht des CT-Entwicklers:
  *  1. Selektiv:    NodeId.attr(Value) bzw. nodeLabel(id,attr,value).
  *  2. Relational:  node(id, attr, ..., attr, ref, ..., ref).
  */

/* ALLES WEITERE HIER NOCH NICHT MIT DEM REST KONSISTENT. */


/* --------------------- */

tree_derived(ID,Parent,Functor,Call):-
    tree_def(Functor,[tree_arg(parent,_)|RestSpecList]),
    length(RestSpecList,RestNum),
    uniqueArgumentList(RestNum,RestArgumentList),
    Call =.. [Functor,ID,Parent|RestArgumentList],
    call(Call).


/** ast_call(?Functor,?ID,?Parent,?Call) 
 *    arg4 is the result of accessing an AST element with functor arg1,
 *    first argument arg2 (ID) and second argument arg3 (Parent).
 */  
ast_call(Functor,ID,Parent,Call) :- 
    tree_fact_with_id_and_parent(Functor,ID,Parent,Call),
    call(Call).                                          % not PEable
 
/** ast_fact_with_id_and_parent(?Functor,?ID,?Parent,?Call) 
 *    arg4 is a most general term for AST elements with functor arg1
 *    whose first argument is unified with arg2 (ID) and 
 *    whose second argument is unified with arg3 (Parent).
 */  
ast_fact_with_id_and_parent(Functor,ID,Parent,Call) :-  % fully PEable
    tree_fact(Functor,Call),
    Call =.. [Functor,ID,Parent|_].
 
/** ast_fact(?Functor,?Call) 
 *    arg2 is a most general term for AST elements with functor arg1
 */          
ast_fact(Functor,Call):-                                % fully PEable
    tree_def(Functor,_argList),
    length(_argList,_arity),
    functor(Call,Functor,_arity).    
