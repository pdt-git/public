
ast_enclosing(Language, encl) :- 
	programmingLanguage(Language). 
	
programmingLanguage(L) :- oopl(L).
programmingLanguage(L) :- xyz(L).

oopl('Java').

 /**
  * Define statements and expressions for all programming languages.
  */

:- multifile statement_type/2.
:- multifile expression_type/2.

consistent(Lang) :-
    programmingLanguage(Lang),
    ast_node_def(Lang,Label,[_Id,_Parent,Encl|_]),
    Encl = ast_args(encl, 1, id, Types),
    correct_enclosing_type(Lang,Label,Types).
  
 /** 
  * correct_enclosing_type(Lang,Label,[methodDefT])
  *
  * Statements can only be contained in methods.
  * Expressions can be contained in methods and field initializers.
  */
    
correct_enclosing_type(Lang,Label,[methodDefT]):-    
    statement_type(Lang,Label).
    
correct_enclosing_type(Lang,Label,Types):-    
    expression_type(Lang,Label),
    ( Types = [methodDefT,fieldDefT]
    ; Types = [fieldDefT,methodDefT]
    ).
 