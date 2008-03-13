:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- op(1100,xfy,'||').
:- op(1100,xfy,'negseq').
:- op(1050,xfy,'&>').
:- op(1000,xfy,'&&').

/*
predicates starting with astseq are ment to operate on elements of sequences.

Seqences are right-recursive chains of binary nodes of the same functor type, 
terminated by an arbitrary other node that does not fit into this schema.
Typical examples would lists or conjunctions. The type of the chain operator is determined
by looking at the arguments parent node.
*/


/*
variables and consistency

After deleting or moving subtrees, the variable set of the affected toplevels may be inconsistent:
1) The toplevel may contain occurances of variables that orginate from another toplevel.
  The predicate fix_variables/2 takes care of those. 
  You MUST run this predicate on all modified toplevels, or else your program may be inconsistent.
2) A toplevel may contain variables that are not used any more. 
  The preducate delete_unused_variables/2 takes care of those.
  Note that occurances in unlinked subtrees, or occurances that were moved into
  other toplevels on which fix_variables/2 has not yet been run will keep variables 
  from beeing deleted.
  
  To avoid confusion:
  - keep track of your garbage. Don't leave unlinked subtrees in the database.
  - always run the FIRST step on all affected toplevels. THEN run the second on all
    affected toplevels.
*/


ctseq(pdt_delete_unused_variables(Toplevel),
	andseq(
		ct(		
			condition(
				pef_ast_query([toplevel=Toplevel,id=Ast]),
				pef_variable_query([ast=Ast,id=Var]),
				\+ pef_variable_occurance_query([variable=Var])
			),
			action(skip)
		),
		orseq(
			ct(
				condition(pef_singleton_query([variable=Var,id=Id])),
				action(pef_singleton_retractall([id=Id]))
			),
			orseq(
				ct(
					condition(pef_no_singleton_query([variable=Var,id=Id])),
					action(pef_no_singleton_retractall([id=Id]))
				),
				ct(
					condition(pef_property_query([pef=Var,id=Id])),
					action(pef_property_retractall([id=Id]))
				)
			)
		)
	)
).

ctseq(pdt_fix_variables(Ast),
   andseq(
       ct(
           condition(               
               %(1)
               fix_vars__nonlocal_occ(AST,Root,_,Name,Occ)
           ),
           action(skip)
       ),
       &>(
            % (2)
           pdt_fix_variables__generate_local(Name,Ast,'rename',Var),
           ct(
               condition(true),
               action( %(3)
                   pef_variable_occurance_retract([id=Occ]),
                   pef_variable_occurance_assert([id=Occ,variable=Var])
               )
           )
       )
   )
).

ctseq(pdt_fix_variables__generate_local(Name,Ast,Mode,Var),
	negseq(
		andseq(
			ct(
				condition(
					pef_variable_query([ast=Ast,name=Name,id=ExistingVar])
				),
				action(skip)
			),
			negseq(
				ct(
					condition(
						Mode==rename,
						fix_vars__unused_name(Ast,Name,NewName),
						pef_reserve_id(pef_variable,Var)
					),
					action(
						pef_variable_assert([id=Var,ast=Ast,name=NewName])
					)
				),
				ct(
					condition(Var=ExistingVar),
					action(skip)
				)
			)
		),
		ct(
			condition(pef_reserve_id(pef_variable,Var)),
			action(pef_variable_assert([id=Var,ast=Ast,name=Name]))
		)
	)
).

ct(pdt_internal__shift_left(N,Parent),
	condition(
		N > 1,
		my_arg(ArgNum,Par,Child),
		ArgNum > N,
		ArgNum2 is Argnum -1
	),
	action(
		pef_arg_retractall([child=Child,parent=Par,num=ArgNum]),
		pef_arg_assert([child=Child,parent=Par,num=ArgNum2]),
	)
).
    
ct(pdt_internal__increment_arity(Node),
	condition(
		pef_term_query([id=Node,name=Name,arity=Arity]),		
		NewArity is Arity + 1
	),
	action(
		pef_term_retractall([id=Node]),
		pef_term_assert([id=Node,name=Name,arity=NewArity]),
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
		pef_term_assert([id=Node,name=Name,arity=NewArity]),
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

ctseq(pdt_internal__ast_unlink__fix_parent(Child),
	(	ct(condition(my_arg(ArgNumm,Parent,Child)
	&+>	(	pdt_internal_decrement_arity(Parent) 
		&+>	pdt_internal__shift_left(N,Arity,Parent)
		)
	
% move node to pseudo-ast
ct(pdt_internal__ast_unlink__move_child(Node,PseudoAST),
	condition(	
		ast_node(Node,AST),
		ast_file(AST,File),					
		pef_reserve_id(pef_pseudo_ast,PseudoAST)
	),
	action(
		pef_arg_retractall([child=Node,parent=Par,num=N]),						
		pef_pseudo_ast_assert([id=PseudoAST,root=Node,file=File])
	)
).

ctseq(pdt_internal__ast_unlink__ensure_root_of_pseudo_ast(Root,PseudoAST),
	(	ct(%if node is the root of an ast linkt to a toplevel, turn the ast into a Pseudo-AST
			condition(
				pef_ast_query([root=Root,id=PseudoAST,toplevel=Toplevel]),
				pef_toplevel_query([id=Toplevel,file=File])
			),
			action(
				pef_ast_retract([id=Id]),
				pef_pseudo_ast_assert([id=Id,root=Root,file=File])
			)
		)		
	&-> ct(%if node is the root of an pseudo ast, do nothing.
			condition(pef_pseudo_ast_query([root=Root,id=PseudoAST])),
			action(skip)
		)
	).

ctseq(pdt_ast_unlink(Node,PseudoAST),
	(	%if Node is an inner Node:
		ct(condition(my_arg(N,Par,Node)),action(skip))
	&+>	(	ct(	% remove link to parent
				condition(true),
				action(
					pef_arg_retractall([child=Node,parent=Par,num=N])						
				)
			) 
		&+>	pdt_internal__ast_unlink__move_child(Node,PseudoAST) 
		 
		&+>	pdt_fix_variables(PseudoAST) 
		&+>	pdt_delete_unused_variables(AST)
		)							
			
	&-> %if Node is a root node:
		pdt_internal__ast_unlink__ensure_root_of_pseudo_ast(Node,PseudoAST)
	)					
).

/*
ast_replace_keep(+Old,+New)
Replace one ast node with another.
First, Old is unlinked, using pdt_ast_unlink.

  
The first argument is unlinked but not deleted.
*/	
ctseq(ast_replace_keep(Old,New),
	%if Old is an inner Node:
	ct(condition(my_arg(N,Par,Node)),action(skip))
	&+>	(	ct(	% unlink old from its parent
				condition(true),
				action(
					pef_arg_retractall([child=Node,parent=Par,num=N])						
				)
			)
			pdt_internal__ast_unlink__move_child(Node,PseudoAST)
		)
)



/*
remove an element from a sequence. 
unlinks the element, but does not delete it.
*/
astseq_remove_keep(Elm):-
	my_arg(Num,Par,Elm),
	my_functor(Par,_Name,2),
	(	Num==1
	->	my_arg(2,Par,Next),
		ast_replace_keep(Par,Next)
	;	my_arg(1,Par,Prev),
		ast_replace_keep(Par,Prev)
	),
	ast_unlink(Elm),
	ast_delete(Par).

astseq_remove(Elm):-
	astseq_remove_keep(Elm),
	ast_delete(Elm).	
/*
Replace one node with another.
First, the second argument is unlinked.
Then unlink the first argument and replace it with the second.
The first argument is unlinked but not deleted.
*/	
ast_replace_keep(Old,New):-
	ast_unlink(New),
	my_arg(Num,Par,Old),
	pef_arg_retractall([child=Old,parent=Par]),
	pef_arg_assert([child=New,num=Num,parent=Par]).

/*
Replace one node with another.
Like replace_keep, but recursively deletes the first argument.
*/	
ast_replace(Old,New):-
	ast_replace_keep(Old,New),
	ast_delete(Old).

	
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
    (	pef_ast_query([id=Ast,toplevel=Toplevel]),
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
	