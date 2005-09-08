
 /**
  *  migrate_args(+Id,+ArgDefsRest,+ArgRest,?SingleValuedAttrsResult)
  * 
  *    Convert the remaining arguments to edges or multivalued
  *    attributes and return the singlevalued attributes in arg4:
  */
 migrate_args(_Id,X,Y,Vals) :-  
    migrate_args(_Id,X,Y,[],Vals).
  
  
  
 /**
  *  migrate_args(+Id,+ArgDefsRest,+ArgRest,+SingleValuedAttrs,?SingleValuedAttrsResult)
  * 
  *    Convert the remaining arguments to edges or multivalued
  *    attributes and return the singlevalued attributes in 
  *    arg5 (SingleValuedAttrsResult).
  *    Ar4 is the list of single valued attributes collected so far.
  */
  
 % Wenn beide Listen leer sind sind wir fertig. Die bisher gesammelten
 % Werte für Attribute mit Kardinalität 1 werden zurückgegeben.
migrate_args(_Id,[],[],Vals,Vals).

migrate_args(_Id,[],[],[]).

% Wenn die Liste der Argumentwerte leer und die der Definitionen noch nicht 
% leer ist kommen nur noch die Sonderfälle. Daraus ergeben sich keine
% neuen Werte für Attribute mit Kardinalität 1.
migrate_args(Id,Defs,[],Vals,Vals) :-  % special cases
    not( Defs = []),
    migrate_special_cases(Defs,Id).
    
 % Attribute with cardinality 1: add its value to AttrVals
migrate_args(Id, 
             [ast_arg(_Label, 1, attr, _Types)|ArgDefsRest],
             [ArgVal|_ArgValsRest],
             AttrVals,
             AttrValsNew
            ) :-  
    migrate_args(Id,ArgDefsRest,[ArgVal|AttrVals],AttrValsNew).
    
 % multivalue attributes: create ast_attr/3 facts 
migrate_args(Id, 
             [ast_arg(Label, Card, attr, _Types)|ArgDefsRest],
             [ArgVals|_ArgValsRest],
             AttrVals,
             AttrValsNew
            ) :- 
    not( Card = 1), 
    migrate_to_ast_attrs(Id,Label,ArgVals),
    migrate_args(Id,ArgDefsRest,AttrVals,AttrValsNew).   
    
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
    
       