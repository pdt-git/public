% Autor: Guenter Kniesel
% Date: 7.9.2004

 /** 
  * Language independent representation of abstract syntax trees (ASTs)
  * in JTransformer. Will be supported as of JTransformer version 2.1.
  * Also called the "new" internal representation. The previously used
  * Java-centric representation is often called the "old" one.
  
  *   ast_node(Id,label).   
      % arg1 is the unique identiy of an AST node of type arg2.
      
  *   ast_attr(Id,attrName,attrValue). 
      % arg3 is one value of the attribute arg2 of the node arg1.
      % Attributes are all the values that are not ids of other nodes.
      % A list containing ids is still considered as an attribute. 
      %
      % The 'true' value of a boolean attribute named 'flag' is 
      % represented by the fact ast_attr(Id, 'flag', 'true').
      % The 'false' value of 'flag' is defined as
      %   ast_attr(Id, 'flag', 'false') :- 
      %      not(ast_attr(Id, 'flag', true)).
      
  *   ast_edge(Id1,Relation,Id2).      
      % Represents a relation of type arg3 between the node with 
      % id arg1 and the node with id arg3.
      
  *   ast_child(Id1,Role1,Id2,Role2).   
      % Represents nontrivial bidirectional relations, characterized 
      % by the fact that the nodes may play different roles in the 
      % relation. The value of arg2 represents  the role played 
      % by arg1 and the value of arg4 represents the role played 
      % by arg2. 
      % By convention the backreference of an AST node to its parent
      % is has the role name 'parent' as arg2. 
  */    
 /* *****************************************************************
    Rationale: 
 
    The above is a compromise between a fully inlined 
    representation, where all attributes and references to other 
    nodes are represented as arguments of ast_node facts and a
    fully factored representation, where the ast_node facts only
    contain the node id and label. In the factored 
    representation ast_attr(Id,attrName,attrValue) and 
    ast_edge(Id1,Role1,Id2,Role2) would be sufficient. The
    ast_flag/2 and ast_edge/3 predicates are special cases.
    
    The arguments for a fully factored representation are its 
    generality, uniformity (no special case treatment necessary),
    and complete independence of argument positions since all
    information is accessed only via argument names.
    
    The argument against a fully factored represenation were 
    fears that it would be inefficient.
    
    We thought that the chosen compromise balances these forces. 
    *************************************************************** */


 /**
  * migrate_factbase/0
  *
  * Converts the Java-centric fact base representation into the 
  * language independent one. After the conversion, the old 
  * facts are deleted.
  * 
  * DELETION OF THE OLD FACTBASE IS CURRENTLY INACTIVATED 
  * (to be enabled after thorough testing). In the transitional
  * period, the deletion must be triggered excplicitly via an 
  * invocation of clear_old_factbase/0.
  */
migrate_factbase :-                    % migrate ast nodes
    print('Migrating nodes and attributes... '),
    astNodeSignature(Functor,Arity),   % <-- JT 2.0.1 predicate
      functor(Head,Functor,Arity),
      clause(Head,true),
      migrate_node(Head),              % <-- this does the real work
     fail.
migrate_factbase :-
    print('Factbase Migration completed. Listing new factbase: '),
    listing(ast_node),
    listing(ast_edge),
    listing(ast_attr),
    listing(ast_flag),
    print('New factbase listing completed').
    % Automatic deletion only after thorough tests of migration:
    % clear_old_factbase. 


clear_old_factbase :-
    print('Deleting old factbase... '),
    astNodeSignature(Functor,Arity),   % <-- JT 2.0.1 predicate
      functor(Call,Functor,Arity),
      retractall(Call),
    fail. 
clear_old_factbase :-
    print('Old factbase cleared.').


 /**
  * migrate_node(+OldNode) 
  *
  * For the fact arg1 from the Java-centric AST representation  
  * a set of equivalent facts in the language independent 
  * representation are added to the factbase. The old fact (arg1)
  * is not removed from the factbase. This is the responsibility
  * of the calling site, which might have reasons not to delete.
  *
  * Example -----------------------------------------------:
  * 
  * In the following the # symbols just indicate positions 
  * that have identities as values.
  *
  * Value of arg1 in old tree_node/1 fact:
  *  methodDefT(#id, #class, 'name', [#param1,...], TYPE, 
  *                                  [#exception1,...], #body)
  *
  * Value of ast_node_def fact describing the AST node conceptually:  
  *  ast_node_def('Java',methodDefT,[
  *   ast_arg(id,      1, id,  [id]), % <-- convention!!!
  *   ast_arg(parent,  1, id,  [classDefT]), 
  *   ast_arg(name,    1, attr,[atomType]),
  *   ast_arg(params,  *, id,  [paramDefT]),
  *   ast_arg(type,    1, attr,[typeTermType]), 
  *   ast_arg(excepts, *, id,  [classDefT]),
  *   ast_arg(body,  0-1, id,  [blockT])
  *  ]).
  * 
  * New facts created for the above input: 
  *  ast_node(#id,methodDefT,'name',TYPE).          
  *  ast_edge(#id,parent,#classDef).
  *  ast_edge(#id,params,#param1).
  *  ...
  *  ast_edge(#id,params,#paramN).
  *  ast_edge(#id,excepts,#exception_1).
  *  ...
  *  ast_edge(#id,params,#exception_N).
  *  ast_edge(#id,body,#body).
  */
migrate_node(OldNode) :- 
      % The old representation contains the id as first and
      % the parent reference as second argument in all nodes:
    OldNode =.. [Fkt,Id,Parent|ArgRest], 
      % Get the metainformation that drives the conversion of
      % nodes of type Fkt in the Java syntax:
    ast_node_def('Java', Fkt, [Id,ParentDef|ArgDefsRest]), 
%      % Convert the parent reference to an ast edge:
%    ParentDef = ast_arg(ArgName,_,_,_),
%    migrate_to_ast_edge(Id,ArgName,Parent), 
      % Convert the remaining arguments to edges or multivalued
      % attributes and return the singlevalued attributes in 
      % arg4 (SingleValuedAttrs):
    migrate_args(Id,ArgDefsRest,ArgRest,SingleValuedAttrs),
      % Create the ast_node fact with the id Id, label Fkt and 
      % one argument for each elem of SingleValuedAttrs:
    migrate_to_ast_node(Id,Fkt,SingleValuedAttrs).
   
   /**  
    * migrate_to_ast_node(+Id,+Fkt,+SingleValuedAttrs)
    *
    * Add the fact "ast_node(Id,Fkt,Attr1,...AttrN) to the Prolog DB.
    * Attr1,...AttrN are the elements of SingleValuedAttrs (arg3).
    */ 
migrate_to_ast_node(Id,Fkt,_attrValues) :-
    NewNode =.. [ast_node,Id,Fkt|_attrValues],
    assert(NewNode,true).
 
 
migrate_to_ast_edge(Id,Label,Id2) :- 
	assert(ast_edge(Id,Label,Id2),true). 

migrate_to_ast_edges(Id,Label,[Val]) :-   % es gibt mindestens den Wert 'null'
    assert(ast_edge(Id,Label,Val),true).
migrate_to_ast_edges(Id,Label,[Val|Vals]) :-
    assert(ast_edge(Id,Label,Val),true),
    migrate_to_ast_edges(Id,Label,Vals).    

 /**
  *  migrate_args(+Id,+ArgDefsRest,+ArgRest,?SingleValuedAttrs)
  * 
  *       % Convert the remaining arguments to edges or multivalued
      % attributes and return the singlevalued attributes in 
      % arg4 (SingleValuedAttrs):
  


migrate_to_ast_attrs(Id,Label,[Val]) :-   % es gibt mindestens den Wert 'null'
    assert(ast_attr(Id,Label,Val),true).
migrate_to_ast_attrs(Id,Label,[Val|Vals]) :-
    assert(ast_attr(Id,Label,Val),true),
    migrate_to_ast_attrs(Id,Label,Vals).    
	   
/* ast_arg(encl, 1, id,  [methodDefT,fieldDefT]) 
    
   *   ast_node(Id,label,attr1,attr2,attr3). % single valued attributes
   *   ast_edge(Id1,Id2,Relation).           % unidirectional references
   *   ast_edge(Id1,Id2,Role1,Role2).        % nontrivial bidirectional references
   *   ast_attr(Id,attrName,attrValue).      % multi valued attributes
   *   ast_flag(Id,attrName).                % flags  
      
*/    

 /**
  * migrate_args(?ArgDefs,?Args,-AttrValues)
  *
  */
 migrate_args(_Id,X,Y,Vals) :-  
    migrate_args(_Id,X,Y,[],Vals).
  
  
  
  
   
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
	
	   
 % ============================================================
  
 % Wenn beide Listen leer sind sind wir fertig. Die bisher gesammelten
 % Werte für Attribute mit Kardinalität 1 werden zurückgegeben.

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
	
	   
/* *******************************************************************
   Sonderbehandlung bei Migration erforderlich für bisherige
   astAttributes (keine Knoten im Baum):
   ***************************************************************** 
   
extendsT(#class, #extendedClass).  
  ---> ast_edge(#class,#extendedClass,extends).
  Wird getriggert durch folgende Attributbeschreibung in classDefT:
  >>>> ast_arg(extends, 1, id, [classDefT])
  
implementsT(#class, #interface) 
  ---> ast_edge(#class,#interface,implements).
  Wird getriggert durch folgende Attributbeschreibung in classDefT:
  >>>> ast_arg(implems, *, id, [classDefT])
  
modifierT(#id, modifierValue) 
  ---> ast_attr(#id, modifier, modifierValue)
  Wird getriggert durch folgende Attributbeschreibung in classDefT,
  methodDefT, fieldDefT, localDefT, ...?... :
  >>>> ast_arg(modifs, *, attr, [atom])

externT(#id)   
  ---> ast_flag(#id,external) 
  Wird getriggert durch folgende Attributbeschreibung in classDefT:
  >>>> ast_arg(extern, 0-1, flag, [])

interfaceT(#class)  
  ---> ast_flag(#class,interface) 
  Wird getriggert durch folgende Attributbeschreibung in classDefT:
  >>>> ast_arg(interf, 0-1, flag, [])
  **************************************************************** */  

migrate_special_cases([], _).
migrate_special_cases([ast_arg(Label, Card, Attr, Types)|ArgDefsRest], Id ) :- 
	migrate_special(OldLabel, Arity, Label),
	atomconcat('migrate_', OldLabel, PredName),
	Call =.. [PredName,Id],
	call(Call),  % rufe passendes prädikat 'migrate_<OldLabel>(Id)' auf
    migrate_special_cases(ArgDefsRest, Id).
    
	
migrate_special(extendsT,    2, extends).
migrate_special(implementsT, 2, implems).
migrate_special(modifierT,   2, hasModif).
migrate_special(externT,     1, isExtern).
migrate_special(interfaceT,  1, isInterf).

migrate_extendsT(Id) :-
    extendsT(Id,Extended),
    migrate_special(extendsT, 2, Relation),
    assert( ast_edge(Id,Relation,Extended), true),
    fail.
migrate_extendsT(Id). 

migrate_implementsT(Id) :-
    implementsT(Id,Extended),
    migrate_special(implementsT, 2, Relation),
    assert( ast_edge(Id,Relation,Extended), true),
    fail.
migrate_implementsT(Id).  

migrate_modifierT(Id) :-
    modifierT(Id,Extended),
    migrate_special(modifierT, 2, Relation),
    assert( ast_attr(Id,Relation,Extended), true),
    fail.
migrate_modifierT(Id).   

migrate_interfaceT(Id) :-
    interfaceT(Id),
    migrate_special(interfaceT, 1, Relation),
    assert( ast_flag(Id,Relation), true),
    fail.
migrate_interfaceT(Id).  

migrate_externT(Id) :-
    externT(Id),
    migrate_special(externT, 1, Relation),
    assert( ast_flag(Id,Relation), true),
    fail.
migrate_externT(Id). 
   


 /* --- ALTERNATIVE DARSTELLUNG der ast_node_defs, DIE AUTOMATISCH GENERIERT WERDEN KÖNNTE
  *     (Erlaubt schnelleren Zugriff auf Argumentinfos)  
  */
  
 /**
  * ast_node_def(?Language, ?AstNodeSignature, ?ArgumentNr, ?ArgName, ?Cardinality, ?ArgKind, ?ArgTypes)
  *               index      index              index       index     
  *
  * The Abstract Syntax Tree (AST) for the language arg1 contains 
  * nodes with signature (functor/arity) arg2 and argument described 
  * by arg3 - arg8. 
  * One AST node for a given language is described by arity-2 facts,
  * since by convention the first two arguments are the node's id 
  * and the reference to the parent node, which are defined at a 
  * language independent level.
  * 
  * arg1 Defined language
  * arg2 A term of the form Label/Arity indicating the node label and its 
  *      total number of arguments (including the two predefined ones).
  * arg3 The number of the argument described by this fact.
  * arg4 The name of the argument described by this fact (usually an atom).
  * arg5 The value 'subtree' indicates that this argument refers to an
  *      AST subtree. Any other value is interpreted as 'noSubtree'. 
  *      For clarity, it is strongly advised to use 'noSubtree' 
  *      explicitly.
  * arg6 The cardinality of the argument described by this fact. 
  *      It is one of 
  *        SCHEMA  | EXAMPLE | EXPLANATION
  *        --------+---------+------------------------------------
  *        *       | *       | Any cardinality, including 0.
  *        From-To | 0-1     | A cardinality range with lower bound
  *                |         | From and uper bound To.
  *        Number  | 1       | Any positive integer denoting a fixed 
  *                |         | number of values (most often 1). 
  *       The cardinality 0 indicates that the value may be 'null'.
  * arg7 The kind of value. It is one of
  *       id   Indicates that the value is the identity of an 
  *            AST node. By convention the first argument of any
  *            AST node is the id of that node and has ArgName 'id'.
  *            All other id arguments refer to other AST nodes. 
  *            We call them 'references'.
  *       attr Indicates that the value is not to be interpreted 
  *            as an id. It can be any legal Prolog term including
  *            compound terms.
  * arg8 is a list of types. Legal types are defined by arg3 of 
  *      ast_node_type/3. 
  *      Every value of the attribute must be from one of the listed 
  *      types. The value 'null', legal if the cardinality includes 0,
  *      is an element of every type.
  *
  * Language independent bottom up traversals of an AST are supported 
  * by the convention that the second argument of every AST
  * node has the name 'parent' and refers to the parent node.
  *
  * TODO:
  * THE FOLLWING EXAMPLE IS NOT YET CONSISTENT WITH THE SPEC ABOVE!!!
  *
  * Example (excerpt from Java AST definition):
  *
  * ast_node_def('Java', applyT,[
  *     ast_arg(id,      1, id,  [id]),              <-- convention!!!
  *     ast_arg(parent,  1, id,  [id]),              <-- convention!!!
  *     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
  *     ast_arg(expr,  0-1, id,  [expressionType]),
  *     ast_arg(name,    1, attr,[atomType]),
  *     ast_arg(args,    1, id,  [expressionType]),
  *     ast_arg(ref,     1, id,  [methodDefT])
  * ]).
  *  
  * This predicate must be defined for every language to be processed
  * by JTransformer. 
  * 
  * TODO:
  * THE ABOVE EXAMPLE MUST BE REPLACED BY A SET OF FACTS OF THE FORM.
  *
  * ast_node_def(?Lang, ?AstNodeSig, ?ArgNr, ?ArgName, ?Cardinality, ?ArgKind, ?ArgTypes)
  */
  
