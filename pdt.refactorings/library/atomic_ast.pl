:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- op(1100,xfy,'||').
:- op(1050,xfy,'&->').
:- op(1050,xfy,'&+>').
:- op(1000,xfy,&&).



/*
variables and consistency

After deleting or moving subtrees, the variable set of the affected toplevels may be inconsistent:
1) The toplevel may contain occurrences of variables that originate from another toplevel.
  The predicate fix_variables/2 takes care of those. 
  
2) A toplevel may contain variables that are not used any more. 
  The predicate delete_unused_variables/2 takes care of those.
  Note that occurrences in unlinked subtrees, or occurrences that were moved into
  other toplevels on which fix_variables/2 has not yet been run will keep variables 
  from being deleted.
  
  To avoid confusion:
  - keep track of your garbage. Don't leave unlinked subtrees in the database.
  - always run the FIRST step on all affected toplevels. THEN run the second on all
    affected toplevels.
*/


/*
pdt_delete_unused_variables(+Ast)
Deletes Variables that are defined for AST but that have no occurrences in this or any other AST.

*/
ctseq(pdt_delete_unused_variables(Ast),
	(	ct(	%find and delete unused variables.
			condition(				
				pef_variable_query([ast=Ast,id=Var]),
				\+ pef_variable_occurance_query([variable=Var])
			),
			action(
				pef_variable_retractall([id=Var])
			)
		)
	&+>	(	ct( %remove any singleton warnings attached to the deleted variable
				condition(pef_singleton_query([variable=Var,id=Id])),
				action(pef_singleton_retractall([id=Id]))
			)			
		|	ct( % remove any non-singleton warnings attached to the deleted variable
				condition(pef_no_singleton_query([variable=Var,id=Id])),
				action(pef_no_singleton_retractall([id=Id]))
			)
		|	ct( % remove any properties attached to the deleted variable
				condition(pef_property_query([pef=Var,id=Id])),
				action(pef_property_retractall([id=Id]))			
			)
		)
	)
).


/*
pdt_fix_variables(+Ast,+NewVars).
Search the given AST for occurrences of variables that are NOT defined in this AST.
Modify found occurrence such that it refers to a variable defined in this AST.

NewVars is either 'true' or 'false'. It determines how name conflicts with 
variables already defined in the given AST should be treated.
If NewVars is 'true', then new variables will be created with names that are made unique by
appending some prefix to it.
If NewVars is 'false', then existing variables will be used. 
  
*/
ctseq(pdt_fix_variables(Ast,NewVars),
   ct( % find occurrences of variables not defined in the AST
       condition( % find occurrences of variables that are deleted or from another toplevel.                 
           fix_vars__nonlocal_occ(Ast,Var,Occ)
       ),
       action(skip)
   )&&
   % find or generated (depending on NewVars) local variables
   pdt_fix_variables__generate_local(Var,Ast,NewVars,LocalVar) &&
   ct( % replace occurrences.
       condition(true),
       action( %(3)
           pef_variable_occurance_retract([id=Occ]),
           pef_variable_occurance_assert([id=Occ,variable=LocalVar])
       )
   )   
   
).


/*
pdt_fix_variables__generate_local(+NonlocalVar,+Ast,+NewVars,-LocalVar).

generate (or find) a variable LocalVar defined in Ast that should be used instead of NonlocalVar.

NewVars is either 'true' or 'false'. It determines how cases should be handled in which
Ast already contains a variable with the same name as NonlocalVar.

If NewVars is 'true',  then LocalVar will be a new variable. Its name is made unique by 
appending some suffix to it.
If NewVars is 'false', then LocalVar will be unified with the Id of the existing variable. 

*/
ctseq(pdt_fix_variables__generate_local(NonlocalVar,Ast,NewVars,LocalVar),
	ct( %determine the name of the non-local variable	
		condition(
			(	pef_variable_query([id=NonlocalVar,name=Name])
			->	true
			;	atom_concat('Deleted',NonlocalVar,Name)
			)
		),
		action(skip)
	) &&
	(	(	ct(	%check if there is an existing variable with the same name
				condition(			
					pef_variable_query([ast=Ast,name=Name,id=ExistingVar])
				),
				action(skip)
			)
		&+>	(	ct(% if a variable with the same name exists and if NewVars==true,
				   % create a new variable with a unique name.
					condition(
						NewVars==true,
						fix_vars__unused_name(Ast,Name,NewName),
						pef_reserve_id(pef_variable,LocalVar)
					),
					action(
						pef_variable_assert([id=LocalVar,ast=Ast,name=NewName])
					)
				)
			&->	ct(% if a variable with the same name exists but NewVars \==true
				   % use the existing variable.
					condition(LocalVar=ExistingVar),
					action(skip)
				)
			)
		)		
	&->	ct( % if there is no variable with this name, create one.
			condition(pef_reserve_id(pef_variable,LocalVar)),
			action(pef_variable_assert([id=LocalVar,ast=Ast,name=Name]))
		)
	)
).


/*
pdt_internal__shift_left(+N,+Parent).

This transformation is intended to close gaps greated by a previous removal of the N-th Argument of Parent.

It moves all arguments of Parent at positions > N one position to the left.

*/
ct(pdt_internal__shift_left(N,Parent),
	condition(
		N > 1,
		my_arg(ArgNum,Par,Child),
		ArgNum > N,
		ArgNum2 is Argnum -1
	),
	action(
		pef_arg_retractall([child=Child,parent=Par,num=ArgNum]),
		pef_arg_assert([child=Child,parent=Par,num=ArgNum2])
	)
).

/*
pdt_internal__shift_right(+N,+Parent).

This transformation is intended make place for a subsequent insertion of a new N-th Argument
to Parent.
It moves all arguments of Parent at positions >= N one position to the right.

*/
ct(pdt_internal__shift_right(N,Parent),
	condition(
		my_arg(ArgNum,Par,Child),
		ArgNum >= N,
		ArgNum2 is Argnum +1
	),
	action(
		pef_arg_retractall([child=Child,parent=Par,num=ArgNum]),
		pef_arg_assert([child=Child,parent=Par,num=ArgNum2])
	)
).

    

ct(pdt_internal__increment_arity(Node),
	condition(
		pef_term_query([id=Node,name=Name,arity=Arity]),		
		NewArity is Arity + 1
	),
	action(
		pef_term_retractall([id=Node]),
		pef_term_assert([id=Node,name=Name,arity=NewArity])
	)					
).

ct(pdt_internal__decrement_arity(Node),
	condition(
		pef_term_query([id=Node,name=Name,arity=Arity]),
		Arity > 1,		
		NewArity is Arity - 1
	),
	action(
		pef_term_retractall([id=Node]),
		pef_term_assert([id=Node,name=Name,arity=NewArity])
	)					
).



/*	
pdt_internal__ast_unlink__move_child(+Node,-PseudoAST).

removes an **inner** node from its parent and makes it the root of
a new pseudo AST.
Pseudo ASTs are handles to temporary trees. 
At any time an ast node is either reachable from either the root
of a normal pef_ast or from that of a pef_pseudo_ast. 
See https://sewiki.iai.uni-bonn.de//research/pdt/developers/architecture/subsystems/pdtcore/projectmodel/schema#ast

This CT fails silently if Node is a root node. 
*/
ct(pdt_internal__ast_unlink__move_child(Node,PseudoAST),
	condition(	
		pef_arg_retractall([child=Node,id=Link]),
		ast_node(Node,AST),
		ast_file(AST,File),					
		pef_reserve_id(pef_pseudo_ast,PseudoAST)
	),
	action(
		pef_arg_retractall([id=Link]),						
		pef_pseudo_ast_assert([id=PseudoAST,root=Node,file=File])
	)
).


/*
pdt_internal__ast_unlink__ensure_root_of_pseudo_ast(+Root,-PseudoAST)

Makes sure that Root is the root of a pseudo ast. If it is the root of normal AST, this AST is replaced by a
pseudo AST. Otherwise, the model is not changed.
PseudoAST is unified with the pseudo AST Root is the root of after this CT was applied.
*/

ctseq(pdt_internal__ast_unlink__ensure_root_of_pseudo_ast(Root,PseudoAST),
	(	ct(%if node is the root of an ast linked to a toplevel, turn the ast into a Pseudo-AST
			condition(
				pef_ast_query([root=Root,id=PseudoAST,toplevel=Toplevel]),
				pef_toplevel_query([id=Toplevel,file=File])
			),
			action(
				pef_ast_retract([id=Id]),
				pef_pseudo_ast_assert([id=Id,root=Root,file=File])
			)
		)		
	&-> ct(%if node is the root of an pseudo ast, do nothing, but unify PseudoAST.
			condition(pef_pseudo_ast_query([root=Root,id=PseudoAST])),
			action(skip)
		)
	)
).


 
/*
pdt_ast_unlink(+Node)

Unlink a subtree from its parent.
Modify the parent node so it is consistent.

The unlinked node will become the root of a pseudo AST that is not linked any toplevel. 
In fact, if the node is the root of an AST, this AST will become a pseudo AST and the link to the toplevel
will be removed.
All variables occuring in the subtree of Node will be replaced by new Variaibles owned by the Pseudo AST.

Both, the new and the old AST will be consistent after this transformation.  
*/

ctseq(pdt_ast_unlink(Node,PseudoAST),
	(	%if Node is an inner Node:
		ct(condition(my_arg(N,Parent,Node)),action(skip))
	&+>	(	%remove link to parent and fix parent.
			ct(	condition(true),action(	pef_arg_retractall([child=Node]))) 
		&+> pdt_internal__decrement_arity(Parent)
		&+> pdt_internal__shift_left(N,Parent)
			
			%move node to pseudo toplevel.
		&+>	pdt_internal__ast_unlink__move_child(Node,PseudoAST)
			
			%fix variables in both ASTs 		 
		&+>	pdt_fix_variables(PseudoAST,false) 
		&+>	pdt_delete_unused_variables(AST)
		)							
			
	&-> %if Node is a root node, make sure it is the root of a pseudo AST.
		pdt_internal__ast_unlink__ensure_root_of_pseudo_ast(Node,PseudoAST)
	)					
).

/*
ctseq(pdt_ast_insert(+ParentNode,+Num,+ChildAST,+NewVars))

Insert the root of the AST ChildAST as the Num-th Argument of ParentNode.
Former arguments at positions >= Num are shifted to the right.
The variable set of the target AST will be adapted: If NewVars is true, this
transformation will make sure that the inserted AST shares no Variables with the 
rest of the Target AST. Otherwise, variables in the inserted AST, that have the same Name as
Variables in the Target AST, will be replaced with the Variables existing in the target AST.

Finally, ChildAST will be deleted, as it is not used anymore.
The Target AST will be consistent after this transformation.
*/
ctseq(pdt_ast_insert(ParentNode,Num,ChildAST,NewVars),
	(	pdt_internal__shift_right(Num,ParentNode) &&
		pdt_internal__increment_arity(ParentNode) &&
		ct(
			condition(
				pef_ast_query([id=ChildAST,root=ChildNode])
			),
			action(
				pef_arg_query([child=ChildNode,parent=ParentNode,num=Num])
			)
		) 
	&+>	(	ct(condition(ast_node(TargetAST,ParentNode)),action(skip)) && 
		 	pdt_fix_variables(TargetAST,NewVars) 
	    |	ct( % the variables of the ChildAST can now be savely deleted
				condition(
					pef_variable_query([ast=ChildAST,id=Var])
				),
				action(
					pef_variable_retractall([id=Var])
				)
			)
		|	( % now we can delete the ChildAST
				ct(
					condition(pef_type(ChildAST,pef_ast)),
					action(pef_ast_retractall([id=ChildAST]))
				)
			&->	ct(
					condition(pef_type(ChildAST,pef_pseudo_ast)),
					action(pef_pseudo_ast_retractall([id=ChildAST]))
				)
			)
		)
	)
).


/*
ctseq(pdt_ast_replace_root(+TargetAST,+SourceAST,-TmpAST))

- replaces the root node of TargetAST with that of SourceAST.
- the old root of TargetAST will be attached to ta new pseudo AST TmpAST
- the SourceAST is not used any more and is therefor removed.
*/

ctseq(pdt_ast_replace_root(TargetAST,NewAST,TmpAST),
	(	(	% replace root of TargetAST.
			ct(	% TargetAST is either attached to a toplevel....
				condition(
					pef_ast_query([id=TargetAST,root=OldRoot,toplevel=Toplevel]),
					ast_root(NewAST,NewRoot)			
				),
				action(
					pef_ast_retractall([id=TargetAST]),
					pef_ast_assert([id=TargetAST,root=NewRoot,toplevel=Toplevel])
				)
			)
		&-> ct(	%.... or it's a pseudo ast
				condition(
					pef_pseudo_ast_query([id=TargetAST,root=OldRoot,file=File]),
					ast_root(NewAST,NewRoot)			
				),
				action(
					pef_pseudo_ast_retractall([id=TargetAST]),
					pef_pseudo_ast_assert([id=TargetAST,root=NewRoot,file=File])
				)
			)
		) &&
		ct(	% attach old root to a new pseudo AST TmpAST
			condition(
				pef_reserve_id(pef_pseudo_ast,TmpAST),
				ast_file(TargetAST,TargetFile)
			),
			action(
				pef_pseudo_ast_assert([id=TmpAST,file=TargetFile,root=OldRoot])
			)
		)
	&+>	(	% Fix variable links in the TargetAST and TmpAST		 
		 	pdt_fix_variables(TargetAST,NewVars) 
	    |	pdt_fix_variables(TmpAST,NewVars)
	    |	ct( % the variables of the SrcAST can now be savely deleted
				condition(
					pef_variable_query([ast=SrcAST,id=Var])
				),
				action(
					pef_variable_retractall([id=Var])
				)
			)
		|	( % next, we can delete the SrcAST
				ct(
					condition(pef_type(SrcAST,pef_ast)),
					action(pef_ast_retractall([id=SrcAST]))
				)
			&->	ct(
					condition(pef_type(SrcAST,pef_pseudo_ast)),
					action(pef_pseudo_ast_retractall([id=SrcAST]))
				)
			)
		|	%finally, delete variables that are not referenced any more
			pdt_delete_unused_variables(TargetAST)
		|	pdt_delete_unused_variables(TmpAST)
		)
	)
).

/*
ast_replace(+Old,+New,+NewVars,-TmpAST)
Replace one ast node with another.
Unlink Old and New,
insert the ast of NEW at original position of Old.

If NewVars is true, this
transformation will make sure that the inserted AST shares no Variables with the 
rest of the Target AST. Otherwise, variables in the inserted AST, that have the same Name as
Variables in the Target AST, will be replaced with the Variables existing in the target AST.

TmpAST is unified with the pseudo ast created for Old.

TODO: 
	This should become the smallest AST transformation directly used within refactorings.
	It needs to be able to deal with epsilon nodes as its first to arguments.
	It needs to make sure that update markers are placed into the right places within the affected ASTs.
 
*/	
ctseq(ast_replace(Old,New,NewVars,TmpAST),
	pdt_ast_unlink(New,NewAST) &&
	(	ct(condition(my_arg(ArgNum,ParentNode,Old)),action(skip)) 
	&+>	%Old is an inner node
		pdt_ast_unlink(Old,TmpAST) &&
		pdt_ast_insert(ParentNode,ArgNum,NewAST,NewVars)
	&-> %Old is a root node.
		(	ct(condition(ast_root(TargetAST,Old)),action(skip))
		&+>	pdt_ast_replace_root(TargetAST,NewAST,TmpAST)
		)
	)
).






/*
remove an element from a sequence. 

(A sequence is a chain of binary terms like a list, or a conjunction)

unlinks the element, but does not delete it.
Expects the parent of Elm to be of arity 2. 
Fails if it is not, or if Elm is a root node.
Otherwise, replaces Elms parent with Elms sibling.
A temoprorary AST with Elms parent as its root and Elm as roots only child 
will be created and returned in TmpAST.
 
*/
ctseq(pdt_ast_seq_remove(Elm,TmpAST),
	ct(	condition(
			my_arg(Num,Par,Elm),
			my_functor(Par,_,2)
		),
		action(
			skip
		)
	) &&
	(	ct(condition(Num==1,my_arg(2,Par,Next)),action(skip))
	&+> ast_replace(Par,Next,false,TmpAST)
	&->	ct(condition(Num==2,my_arg(1,Par,Next)),action(skip))&&
		ast_replace(Par,Prev,false,TmpAST)
	)	
).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Left Todo:
	
ast_delete(Node):-
	forget_subtree(Node).	
	
forget_subtree(Id):-
    pef_variable_occurance_query([id=Id,variable=Ref]),
    !,
    %pef_variable_occurance_retractall([id=Id]),
    pef_delete(Id),
    % if this was the last occurance, remove the variable.
	(	pef_variable_occurance_query([variable=Ref])
	->	true
	;	%pef_variable_retractall([id=Ref])
		pef_delete(Ref)
	),
	pef_arg_retractall([child=Id]).
    
forget_subtree(Id):-
	forall(
		pef_arg_query([parent=Id,child=ChildId]),
		forget_subtree(ChildId)
	),
	%pef_term_retractall([id=Id]),
	pef_delete(Id),
	pef_arg_retractall([child=Id]).  	
	
%% mark_toplevel_modified(+Toplevel)
% mark a toplevel fact as 'modified'. 
% Do this after you modified a toplevel.
mark_toplevel_modified(Toplevel):-
    (	pef_modified_toplevel_query([toplevel=Toplevel])
    ->	true
    ;	pef_modified_toplevel_assert([toplevel=Toplevel])
    ),
    (	pef_toplevel_query([id=Toplevel,file=File])
    ->	mark_file_modified(File)
    ;	true
    ).

mark_file_modified(File):-
    (	pef_modified_file_query([file=File])
    ->	true
    ;	pef_modified_file_assert([file=File])
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% no side effects below this line.

fix_vars__nonlocal_occ(Ast,Var,Occ):-
	ast_root(Ast,Root),
    subtree(Root,Occ),
    pef_variable_occurance_query([id=Occ,variable=Var]),
    (	pef_variable_query([id=Var,name=Name,ast=Ast2])
    ->	Ast \== Ast2
    ;	true
    ).

fix_vars__unused_name(Ast,Base,Name):-
    pef_variable_query([ast=Ast,name=Base]),
    !,
    atom_concat(Base,'_X',NewBase),
    fix_vars__unused_name(Ast,NewBase,Name).
fix_vars__unused_name(_Ast,Name,Name).


ast_node(Ast,Node):-
    (	nonvar(Node)
    ->	subtree_up(Root,Node),
    	pef_ast_query([root=Root,id=Ast])
    ;	pef_ast_query([id=Ast,root=Root]),
    	subtree_down(Root,Node)
    ).

ast_file(Ast,File):-
    (	pef_ast_query([id=Ast,toplevel=Toplevel])
	->	pef_toplevel_query([id=Toplevel,file=File])
	;	pef_pseudo_ast_query([id=Ast,file=File])
	).

subtree(A,B):-
    (	nonvar(B)
    ->	subtree_up(A,B)
    ;	subtree_down(A,B)
    ).
subtree_down(Ast,Ast).
subtree_down(Ast,Subtree):-
	pef_arg_query([parent=Ast,child=Child]),
	subtree_down(Child,Subtree).    
subtree_up(Ast,Ast).
subtree_up(Ast,Subtree):-
	pef_arg_query([parent=Parent,child=Subtree]),
	subtree_up(Ast,Parent).    


my_arg(Num,Parent,Child):-
    pef_arg_query([child=Child,parent=Parent,num=Num]).

my_functor(Node,Name,Arity):-
	pef_term_query([id=Node,name=Name,arity=Arity]).    

my_var(Node):-
	pef_type(Node,pef_variable_occurance).    
	