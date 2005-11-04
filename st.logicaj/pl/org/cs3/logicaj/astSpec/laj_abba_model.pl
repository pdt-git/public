/**
 * Specification of the LogicAJ abba nodes.
 *
 */
abba:nodetype(laj_aspect).
/**
 * Generic advice or generic introduction.
 *
 *  e.g.: before(..) : ....  {} 
 */
abba:nodetype(laj_effect).
/**
 * Show error or warning construct.
 *
 *  e.g.: declare warning ....
 */
abba:nodetype(laj_analysis).

/**
 * Pointcut designator or literal
 * in pointcut definition.
 *
 * e.g.: ... call(* *.*(..)) ...
 */
abba:nodetype(laj_pcd).

/**
 * Pointcut declaration.
 *
 * e.g.: pointcut namedpc(..) : ...
 */
abba:nodetype(laj_pc_dec).

/**
 * Aspect constructs body element.
 *
 * e.g. ... m(); ...
 */
abba:nodetype(laj_body).


abba:edgetype(parent, laj_effect, aspect).
abba:edgetype(parent, laj_analysis, aspect).
abba:edgetype(parent, laj_pcd, aspect).
abba:edgetype(parent, laj_pc_dec, aspect).

abba:propertytype(laj_effect, kind(atom)).
abba:propertytype(laj_analysis, kind(atom)).

abba:propertytype(laj_effect, position(int, int)).
abba:propertytype(laj_analysis, position(int, int)).
abba:propertytype(laj_pcd, position(int, int)).
abba:propertytype(laj_pc_dec, position(int, int)).
abba:propertytype(laj_aspect, position(int, int)).

abba:propertytype(laj_aspect, file(atom)).

delete_logicaj_abba_model :-
  forall((
      abba:node(ID, Kind, Name), 
      logicaj_abba_node_type(Kind)
    ),(
      retract(abba:node(ID, Kind, Name)),
      retractall(abba:property(ID, _)),
      retractall(abba:edge(_,_,ID, _)),
      retractall(abba:edge(_,_,_, ID))
    )).
      
logicaj_abba_node_type(laj_effect).
logicaj_abba_node_type(laj_analysis).
logicaj_abba_node_type(laj_pcd).
logicaj_abba_node_type(laj_pc_dec).
logicaj_abba_node_type(laj_body).
logicaj_abba_node_type(laj_aspect).

