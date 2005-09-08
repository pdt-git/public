 /**
  * migrate_args(+Id,+ArgDefs,+Args,?SingleValuedAttributes).
  *
  */

migrate_args(_Id,[],[],[]).

% Wenn die Liste der Argumentwerte leer und die der Definitionen noch nicht 
% leer ist kommen nur noch die Sonderfälle. Daraus ergeben sich keine
% neuen Werte für Attribute mit Kardinalität 1.
migrate_args(Id,Defs,[],[]) :-  % special cases
    not( Defs = []),
    migrate_special_cases(Defs,Id).
    
 % Attribute with cardinality 1: add its value to AttrVals
migrate_args(Id, 
             [ast_arg(_Label, 1, attr, _Types)|ArgDefsRest],
             [ArgVal|ArgValsRest],
             [ArgVal|AttrValsNew]
            ) :-  
    migrate_args(Id,ArgDefsRest,ArgValsRest,AttrValsNew).
    
 % multivalue attributes: create ast_attr/3 facts 
migrate_args(Id, 
             [ast_arg(Label, Card, attr, _Types)|ArgDefsRest],
             [ArgVals|ArgValsRest],
             AttrValsNew
            ) :- 
    not( Card = 1), 
    migrate_to_ast_attrs(Id,Label,ArgVals),
    migrate_args(Id,ArgDefsRest,ArgValsRest,AttrValsNew).   
    
  % Bei referenzen werden immer edges angelegt, egal wie die 
  % Kardinalität ist.
 migrate_args(Id, 
             [ast_arg(Label, Card, id, Types)|ArgDefsRest],
             [ArgVals|ArgValsRest],
             AttrVals,
             AttrValsNew
            ) :- 
    migrate_to_ast_edges(Id,Label,ArgVals),
    migrate_args(Id,ArgDefsRest,AttrVals,AttrValsNew). 